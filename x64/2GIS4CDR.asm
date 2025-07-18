format PE64 GUI 4.0 DLL as 'cpg'
entry DllEntryPoint

include 'encoding\win1251.inc'
include 'win64w.inc'
include 'EQUATES\WSOCK32.INC'
include 'CorelDraw.inc'
include '..\API.inc'

prologue@proc equ static_rsp_prologue
epilogue@proc equ static_rsp_epilogue
close@proc equ static_rsp_close

include 'WebSocket.inc'

DllEntryPoint: ;hinstDLL,fdwReason,lpvReserved
  mov eax,TRUE
ret

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov qword[rcx],IPlugin
  mov eax,256
ret

align 16
proc ProgressThread Parent
  mov      rbx,rcx
  invoke   InitCommonControls
  invoke   GetWindowRect,rbx,Rect
  movq     xmm0,qword[Rect.right]
  movq     xmm1,qword[Rect.left]
  psubd    xmm0,xmm1
  cvtdq2ps xmm2,xmm0
  mulps    xmm2,dqword[ProgressAspect]
  cvtps2dq xmm2,xmm2
  psubd    xmm0,xmm2
  psrld    xmm0,1
  paddd    xmm0,xmm1
  movq     qword[Rect.left],xmm0
  movq     qword[Rect.right],xmm2
  invoke   CreateWindowExW,0,strmsctls_progress32,0,WS_VISIBLE+WS_POPUP+WS_CHILD+PBS_SMOOTH,[Rect.left],[Rect.top],[Rect.right],[Rect.bottom],rbx,0,0,0
  mov      [ProgressBar],rax
  invoke   SendMessageW,rax,PBM_SETRANGE,0,1024 shl 16
  mov      r10d,[Rect.bottom]
  sub      r10,20
  shr      r10,1
  add      r10d,[Rect.top]
  invoke   CreateWindowExW,WS_EX_LAYERED,strSTATIC,strInit,WS_VISIBLE+SS_CENTER+WS_POPUP,[Rect.left],r10,[Rect.right],20,[ProgressBar],0,0,0
  mov      [ProgressText],rax
  invoke   GetSysColor,COLOR_3DFACE
  invoke   SetLayeredWindowAttributes,[ProgressText],eax,0,LWA_COLORKEY
  @@:invoke GetMessageW,msg,0,0,0
     invoke DispatchMessageW,msg
  jmp @b
endp

QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov qword[r8],IPlugin
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
GetIDsOfNames:    ; this,IID,Names,NameCount,LocaleID,DispIDs
  mov eax,E_NOTIMPL
ret

proc Invoke uses rbx rbp rdi rsi r12 r13 r14,this,DispID,IID,LocaleID,Flags,Params,VarResult,ExcepInfo,ArgErr
local _xmm6: DQWORD
local _xmm7: DQWORD
  cmp rdx,OnPluginCommand
  je .OnPluginCommand
    cmp rdx,OnUpdatePluginCommand
    jne .exit
      mov      rdx,[Params]
      mov      rdx,[rdx+DISPPARAMS.rgvarg]
      mov      rax,[rdx+sizeof.VARIANT*2+VARIANT.data]
      movdqu   xmm0,[rax]
      pcmpeqb  xmm0,dqword[str2GIS4CDR]
      pmovmskb eax,xmm0
      inc      ax
      jne    .exit
        mov     rbx,[rdx+sizeof.VARIANT*1+VARIANT.data]
        mov     [tmp],0
        cominvk CorelApp,Get_ActiveDocument,tmp
        cmp     [tmp],0
        mov     word[rbx],0
        je @f
          mov     word[rbx],1
          comcall [tmp],IVGDocument,Release
        @@:
        jmp .exit

 .OnPluginCommand:
  movdqa   [_xmm6],xmm6
  movdqa   [_xmm7],xmm7
  mov      rax,[Params]
  mov      rax,[rax+DISPPARAMS.rgvarg]
  mov      rax,[rax+VARIANT.data]
  movdqu   xmm0,[rax]
  pcmpeqb  xmm0,dqword[str2GIS4CDR]
  pmovmskb eax,xmm0
  inc      ax
  jne .exit
  cominvk CorelApp,Set_Optimization,1
  cominvk CorelApp,Get_ActiveWindow,tmp
  mov     rbx,[tmp]
  comcall rbx,IVGWindow,Get_Handle,CorelWndHandle
  comcall rbx,IVGWindow,Release
  cominvk CorelApp,Get_ActiveDocument,CorelDoc
  cominvk CorelDoc,Set_Unit,cdrTenthMicron
  cominvk CorelDoc,BeginCommandGroup,str2GIS4CDR
  cominvk CorelApp,CreateCurve,[CorelDoc],Curve
  cominvk Curve,GetCurveInfo,CurveInfo
  mov     rax,[CurveInfo]
  movdqu  xmm0,dqword[rax+SAFEARRAY.pvData]
  movdqa  dqword[CurveData],xmm0
  cominvk CorelDoc,Get_ActivePage,tmp
  mov     rsi,[tmp]
  comcall rsi,IVGPage,Get_Layers,tmp
  mov     rdi,[tmp]
  mov     [GraphicsLayer],0
  mov     [LabelsLayer],0
  comcall rdi,IVGLayers,Find,strGraphics,GraphicsLayer
  comcall rdi,IVGLayers,Find,strLabels,LabelsLayer
  comcall rdi,IVGLayers,Release
  cmp     [GraphicsLayer],0
  jne @f
    comcall rsi,IVGPage,CreateLayer,strGraphics,GraphicsLayer
  @@:
  cmp     [LabelsLayer],0
  jne @f
    comcall rsi,IVGPage,CreateLayer,strLabels,LabelsLayer
  @@:
  comcall rsi,IVGPage,Release
  invoke CreateThread,0,0,ProgressThread,[CorelWndHandle],0,0
  mov    [ProgressThreadHandle],rax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Initializing WebSocket server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke WSAStartup,$0202,buf
  invoke socket,AF_INET,SOCK_STREAM,IPPROTO_TCP
  mov    [Sock],rax
  mov    [sock_addr.sin_port],$8080
  @@:inc    [sock_addr.sin_port]
     invoke bind,[Sock],sock_addr,sizeof.sockaddr_in
     test   rax,rax
  jne @b
  invoke listen,[Sock],SOMAXCONN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Generating html-file with public api-key and execute it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  mov    rbx,strErrNetwork
  invoke InternetOpen,0,INTERNET_OPEN_TYPE_PRECONFIG,0,0,0
  test   rax,rax
  je     .error
  mov    [hSession],rax
  invoke InternetConnect,rax,str2gis.com,443,0,0,INTERNET_SERVICE_HTTP,0,0
  test   rax,rax
  je     .error
  mov    [hConnect],rax
  invoke HttpOpenRequest,[hConnect],strGET,strBackSlash,strHTTP,0,types,INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS+INTERNET_FLAG_IGNORE_CERT_CN_INVALID+INTERNET_FLAG_IGNORE_CERT_DATE_INVALID+INTERNET_FLAG_NO_AUTH+INTERNET_FLAG_KEEP_CONNECTION+INTERNET_FLAG_SECURE,0
  test   rax,rax
  je     .error
  mov    [hRequest],rax
  invoke HttpSendRequest,rax,0,0,0,0
  test   rax,rax
  je     .error
  mov    [tmp],4
  mov    rbx,strErrKey
  .ReadMainPage:
     invoke InternetReadFile,[hRequest],buf+16,sizeof.buf-16,tmp
     cmp    dword[tmp],0
     je     .error
     xor    edx,edx
     movdqa xmm0,dqword[strtaVectorTilesKey]
     mov    ecx,sizeof.buf-16
     @@:movdqu   xmm1,dqword[buf+edx]
        inc      edx
        pcmpeqb  xmm1,xmm0
        pmovmskb eax,xmm1
        inc      ax
     loopne @b
     mov      eax,dword[tmp]
     movdqu   xmm0,dqword[buf+eax-16]
     movdqa   dqword[buf],xmm0
  jne .ReadMainPage
  lea    rsi,[buf+rdx+18]
  lea    rcx,[buf+rax]
  mov    rdi,buf
  sub    rcx,rsi
  rep    movsb
  lea    esi,[edx+18]
  invoke InternetReadFile,[hRequest],edi,esi,tmp
  xor    esi,esi
  @@:inc esi
     cmp [buf+esi],'"'
  jne @b
  mov    rbx,strErrFile
  invoke GetTempPathW,sizeof.HTMLFileName,HTMLFileName
  movdqu xmm0,dqword[str2GIS.htm]
  movdqu dqword[HTMLFileName+eax*2],xmm0
  mov    [HTMLFileName+eax*2+16],0
  invoke CreateFileW,HTMLFileName,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0
  cmp    rax,INVALID_HANDLE_VALUE
  je     .error;
  mov    rbx,rax
  invoke WriteFile,rax,HTMLBegin,sizeof.HTMLBegin,tmp,0
  invoke WriteFile,rbx,buf,esi,tmp,0
  movzx  eax,[sock_addr.sin_port]
  rol    ax,8
  invoke wsprintfA,buf,fmt,eax
  invoke WriteFile,rbx,buf,eax,tmp,0
  invoke WriteFile,rbx,JavaScript,sizeof.JavaScript,tmp,0
  invoke WriteFile,rbx,HTMLEnd,sizeof.HTMLEnd,tmp,0
  invoke CloseHandle,rbx
  mov    rbx,strErrHTML
  invoke ShellExecuteW,0,stropen,HTMLFileName,0,0,SW_SHOW
  cmp    rax,33
  jl     .error
  invoke SendMessageW,[ProgressText],WM_SETTEXT,0,strChooseTiles

  invoke VirtualAlloc,0,MAX_SHAPES*sizeof.TShape,MEM_COMMIT,PAGE_READWRITE
  mov    [Shapes],rax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Websocket handshake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  mov    [tmp],sizeof.sockaddr_in
  invoke accept,[Sock],sock_addr,tmp
  mov    [Sock2],rax
  invoke recv,rax,buf,sizeof.buf,0
  mov    rdi,buf+4
  xor    eax,eax
  @@:inc rdi
     cmp dword[rdi-4],'ey: '
  jne @b
  @@:inc eax
     cmp byte[rdi+rax],13
  jne @b
  movdqu xmm0,dqword[strWebSocketGUID]
  movdqu xmm1,dqword[strWebSocketGUID+16]
  mov    edx,dword[strWebSocketGUID+32]
  movdqu [rdi+rax],xmm0
  movdqu [rdi+rax+16],xmm1
  mov    [rdi+rax+32],edx
  SHA1Base64 rdi,rax+36,WebsockResponce+97
  invoke send,[Sock2],WebsockResponce,129,0

  mov  [ShapesCount],0
  pxor xmm7,xmm7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Websocket data receiving loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .Main:
    xor r13,r13
    ;Reading Websocket packet
    .WebSocketRead:
      stdcall ReadSock,WebSockHeader,6
      movzx   edx,[WebSockHeader.header]
      and     edx,15
      cmp     edx,8
      jne @f
        invoke send,[Sock2],WebSockHeader,eax,0
        jmp    .CloseSock
      @@:
      movzx   ebx,byte[WebSockHeader.header+1]
      and     ebx,127
      mov     r12d,[WebSockHeader.tiny.mask]
      cmp     ebx,126
      jne @f
        stdcall ReadSock,WebSockHeader+6,2
        movzx   ebx,[WebSockHeader.short.len]
        rol     bx,8
        mov     r12d,[WebSockHeader.short.mask]
      @@:
      cmp     ebx,127
      jne @f
        stdcall ReadSock,WebSockHeader+6,8
        mov     rbx,[WebSockHeader.long.len]
        bswap   rbx
        mov     r12d,[WebSockHeader.long.mask]
      @@:
      lea    rdx,[rbx+r13]
      invoke VirtualAlloc,0,rdx,MEM_COMMIT,PAGE_READWRITE
      mov    [tmp],rax
      test   r13,r13
      je @f
        mov    rcx,r13
        mov    rsi,[WebSocketData]
        mov    rdi,[tmp]
        rep    movsb
        invoke VirtualFree,[WebSocketData],0,MEM_RELEASE
      @@:
      mov     rbp,[tmp]
      mov     [WebSocketData],rbp
      add     rbp,r13
      stdcall ReadSock,rbp,rbx
      lea     rcx,[rbx+3]
      shr     rcx,2
      @@:xor [rbp+rcx*4-4],r12d
      loop @b
      add     r13,rbx
      test    [WebSockHeader.header],$80
    je .WebSocketRead

    mov      rbp,[WebSocketData]
    mov      ebx,[rbp+TDataHeader.Layer]
    mov      [Layer],ebx
    invoke   SendMessageW,[ProgressText],WM_SETTEXT,0,[LayerProgress+ebx*8]
    movsd    xmm6,[dbl_30000000]
    cvtsi2sd xmm0,[rbp+TDataHeader.Width]
    divsd    xmm6,xmm0
    shufpd   xmm6,xmm6,0
    mov      r12d,[rbp+TDataHeader.NumObjects]
    mov      edx,[rbp+TDataHeader.Progress]
    mov      [Progress],edx
    cvtsi2ss xmm0,edx
    cvtsi2ss xmm1,r12d
    subss    xmm0,xmm7
    divss    xmm0,xmm1
    movss    [ProgressIncrement],xmm0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Labels processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    cmp ebx,3
    jne .Graphics
      lea   rbx,[rbp+sizeof.TDataHeader+r12*8]
      add   rbp,sizeof.TDataHeader
      @@:mov      edi,[rbx] ;string length
         shl      dword[rbx],1
         mov      eax,16
         lea      edx,[edi+15]
         cmp      eax,edi
         cmova    eax,edi
         shr      edx,4
         movd     xmm1,eax
         pinsrd   xmm1,edx,1
         movq     xmm3,[rbp]
         cvtdq2pd xmm1,xmm1
         cvtdq2pd xmm3,xmm3
         mulpd    xmm1,xmm6
         mulpd    xmm3,xmm6
         mulpd    xmm1,dqword[LabelAspect]
         movapd   xmm2,xmm1
         mulpd    xmm2,dqword[dbl_05]
         subpd    xmm3,xmm2
         subpd    xmm3,dqword[dbl_15000000]
         addpd    xmm1,xmm3

         add      rbx,4
         movhlps  xmm2,xmm1
         movhlps  xmm0,xmm3
         cominvk  LabelsLayer,CreateParagraphText,xmm1,xmm2,xmm3,xmm0,rbx,0,0,strArial,12,0,0,0,cdrCenterAlignment,tmp
         mov      rsi,[tmp]
         comcall  rsi,IVGShape,Get_Text,tmp
         comcall  rsi,IVGShape,Release
         mov      rsi,[tmp]
         comcall  rsi,IVGText,FitTextToFrame
         comcall  rsi,IVGText,Release

         cvtss2si r8d,xmm7
         invoke   SendMessageW,[ProgressBar],PBM_SETPOS,r8,0
         addss    xmm7,[ProgressIncrement]
         add      rbp,8
         lea      rbx,[rbx+rdi*2]
         dec      r12
      jne @b
      jmp .NextObject
    .Graphics:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Graphics processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov      rax,r12
    shl      eax,4    ;size of Objects
    sub      r13,sizeof.TDataHeader
    sub      r13,rax
    shr      r13,3
    lea      rdx,[rbp+sizeof.TDataHeader]
    mov      [Objects],rdx
    add      rdx,rax
    shl      r13,5
    mov      [Points],rdx
    lea      rdx,[r13+r13*2]
    invoke   VirtualAlloc,0,rdx,MEM_COMMIT,PAGE_READWRITE
    mov      rdx,[CurveInfo]
    mov      [CurveElements],rax
    mov      [rdx+SAFEARRAY.pvData],rax
    lea      rax,[rax+r13*2]
    mov      [VertexArray],rax
    shr      r13,2
    add      rax,r13
    mov      [Polygon],rax

    ;Scaling graphics to fit 3x3m area
    mov      rax,[Points]
    movdqa   xmm2,dqword[dbl_15000000]
    @@:cvtpi2pd xmm0,[rax+r13-8]
       mulpd    xmm0,xmm6
       subpd    xmm0,xmm2
       cvtpd2dq xmm0,xmm0
       movq     [rax+r13-8],xmm0
       sub      r13,8
    jne @b

    dec      r12
    .ObjectsProcessing:
       xor  r14,r14
       mov  rax,r12
       shl  eax,4
       add  rax,[Objects]
       mov  esi,[rax+TObject.Start]
       shl  esi,3
       add  rsi,[Points]
       mov  ecx,[rax+TObject.Count]
       mov  rdi,[VertexArray]
       mov  [VertexArrayLen],ecx
       rep  movsq

       mov  rdi,[Polygon]
       mov  rsi,[VertexArray]
       .VertexArray2CorelCurve:
         ;Merging triangles into a polygon
         sub      [VertexArrayLen],3
         mov      r13,4
         mov      ebx,[VertexArrayLen]
         movdqu   xmm0,[rsi+rbx*8]
         mov      rax,[rsi+rbx*8+16]
         movdqu   [rdi],xmm0
         mov      [rdi+16],rax
         movq     [rdi+24],xmm0
         jmp      .StartMerge
         .Merge:
           mov      ebp,2
           .Tri1:
             lea      ecx,[r13*8-8]
             .Tri2:
               lea      edx,[ebx+ebp]
               movzx    eax,[IndexPlus1+ebp]
               movdqu   xmm0,[rdi+rcx-8]
               add      eax,ebx
               movdqu   xmm1,[rsi+rdx*8-8]
               movq     xmm2,[rsi+rax*8]
               movsd    xmm1,xmm2
               pcmpeqd  xmm0,xmm1
               pmovmskb eax,xmm0
               inc      ax
               jne .NextTriangle
                 mov    rax,r13
                 lea    eax,[eax*8-8]
                 mov    rdx,[Polygon]
                 add    rdx,rcx
                 sub    eax,ecx
                 @@:mov r8,[rdx+rax]
                    mov [rdx+rax+8],r8
                    sub  eax,8
                 jns @b
                 movzx   eax,[IndexPlus2+ebp]
                 sub    [VertexArrayLen],3
                 add    eax,ebx
                 inc    r13
                 mov    r8,[rsi+rax*8]
                 mov    eax,[VertexArrayLen]
                 mov    [rdx],r8
                 movdqu xmm0,[rsi+rax*8]
                 mov    r8,[rsi+rax*8+16]
                 movdqu [rsi+rbx*8],xmm0
                 mov    [rsi+rbx*8+16],r8
                 mov    ebx,eax
                 jmp .StartMerge
               .NextTriangle:
               sub ecx,8
             jne .Tri2
             dec ebp
           jns .Tri1
           .StartMerge:
           sub ebx,3
         jns .Merge

         ;For roads (lines) skip arrowhead
         cmp [Layer],1
         jne @f
           cmp r13,5
           jl .continue
         @@:

         ;Removing bridges from polygons with holes
         mov rdi,[Polygon]
         .RemoveBridges:
           lea rbx,[r13-1]
           mov eax,ebx
           .Right:
             xor edx,edx
             .Left:mov ecx,eax
                   sub ecx,ebx
                   or  ecx,edx
                   je .skip
                     mov r8,[rdi+rax*8]
                     cmp r8,[rdi+rdx*8]
                     jne .skip
                       lea ecx,[ebx+1]
                       sub ecx,eax
                       mov rbp,rdi
                       @@:mov r8,[rbp+rax*8]
                          mov [rbp+rdx*8],r8
                          add  rbp,8
                       loop @b
                       sub edx,eax
                       add r13,rdx
                       jmp .RemoveBridges
                   .skip:
                   inc  edx
                   cmp  eax,edx
             jne .Left
             dec eax
           jne .Right

         ;Generating data for PutCurveInfo
         lea edx,[ebx+1]
         mov rbp,r14
         add r14,rdx
         shl ebp,5
         shl edx,5
         add rbp,[CurveElements]
         shl ebx,3
         @@:cvtpi2pd xmm0,[rdi+rbx]
             movupd  dqword[rbp+rbx*4+CurveElement.PositionX],xmm0
             mov     [rbp+rbx*4+CurveElement.ElementType],cdrElementLine
             mov     [rbp+rbx*4+CurveElement.NodeType],cdrCuspNode
             mov     [rbp+rbx*4+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             sub     ebx,8
         jns @b
         mov [rbp+CurveElement.ElementType],cdrElementStart
         mov [rbp+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
         mov [rbp+rdx+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed

         .continue:
         cmp  [VertexArrayLen],0
       jg .VertexArray2CorelCurve

       ;Creating shape and add it to list
       mov      rax,[CurveInfo]
       mov      [rax+SAFEARRAY.rgsabound.cElements],r14d
       cominvk  Curve,PutCurveInfo,CurveInfo,r14,tmp
       mov      ebx,[ShapesCount]
       shl      ebx,4
       add      rbx,[Shapes]
       cominvk  GraphicsLayer,CreateCurve,[Curve],addr rbx+TShape.Shape
       mov      rdx,r12
       shl      edx,4
       add      rdx,[Objects]
       mov      rax,qword[rdx+TObject.Color]
       mov      qword[rbx+TShape.Color],rax

       ;For roads do break apart with weld
       cmp      [Layer],1
       jne .NoRoad
         invoke  SendMessageW,[ProgressText],WM_SETTEXT,0,strProcessRoads
         comcall [rbx+TShape.Shape],IVGShape,BreakApartEx,tmp
         comcall [rbx+TShape.Shape],IVGShape,Release
         mov     rsi,[tmp]
         comcall rsi,IVGShapeRange,Get_Count,varInt.data
         comcall rsi,IVGShapeRange,Get_Item,varInt,tmp
         mov     rbp,[tmp]
         dec     [varInt.data]
         jle .SimpleShape
           @@:comcall rsi,IVGShapeRange,Get_Item,varInt,tmp
              mov     rdi,[tmp]
              comcall rbp,IVGShape,Weld,rdi,0,0,tmp
              comcall rdi,IVGShape,Release
              comcall rbp,IVGShape,Release
              mov     rbp,[tmp]
              dec     [varInt.data]
           jne @b
         .SimpleShape:
         mov     [rbx+TShape.Shape],rbp
         comcall rsi,IVGShapeRange,Release
       .NoRoad:

       inc      [ShapesCount]
       cvtss2si r8d,xmm7
       invoke   SendMessageW,[ProgressBar],PBM_SETPOS,r8,0
       addss    xmm7,[ProgressIncrement]

       ;if Shapes array is overfilled just skip remeining data
       cmp      [ShapesCount],MAX_SHAPES
       jne @f
         invoke VirtualFree,[CurveElements],0,MEM_RELEASE
         invoke VirtualFree,[WebSocketData],0,MEM_RELEASE
         invoke SendMessageW,[ProgressBar],PBM_SETPOS,1024,0
         jmp    .CloseSock
       @@:

       dec      r12
    jns .ObjectsProcessing

    .NextObject:
    cvtsi2ss xmm7,[Progress]
    invoke   VirtualFree,[CurveElements],0,MEM_RELEASE
    invoke   VirtualFree,[WebSocketData],0,MEM_RELEASE
    mov      [CurveElements],0
  jmp .Main
  .CloseSock:
  invoke closesocket,[Sock2]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sorting shapes by depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke SendMessageW,[ProgressText],WM_SETTEXT,0,strSorting
  mov    edi,[ShapesCount]
  shl    edi,4
  je     .quit
  lea    r12,[rdi-16]
  mov    rbp,[Shapes]
  sub    edi,16
  .Sort:
    mov ebx,edi
    mov eax,[rbp+rdi+TShape.Depth]
    lea edx,[edi-16]
    .Sort2:
      cmp eax,[rbp+rdx+TShape.Depth]
      jl @f
        mov ebx,edx
        mov eax,[rbp+rdx+TShape.Depth]
      @@:
      sub edx,16
    jns .Sort2
    movdqa  xmm0,[rbp+rbx]
    movdqa  xmm1,[rbp+rdi]
    movdqa  [rbp+rdi],xmm0
    movdqa  [rbp+rbx],xmm1
    comcall [rbp+rdi+TShape.Shape],IVGShape,OrderToFront
    sub     edi,16
  jne .Sort
  comcall [rbp+TShape.Shape],IVGShape,OrderToFront

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Welding tiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke SendMessageW,[ProgressText],WM_SETTEXT,0,strProcessTiles
  .WeldTiles:
    mov     rsi,[rbp+r12+TShape.Shape]
    cmp     [rbp+r12+TShape.Depth],-1
    je .AlmostDone
      lea rdi,[r12-16]
      .Welding:
        mov rax,qword[rbp+r12+TShape.Color] ;if color and depth are equal - weld shapes
        cmp rax,qword[rbp+rdi+TShape.Color]
        jne @f
          comcall rsi,IVGShape,Weld,[rbp+rdi+TShape.Shape],0,0,tmp
          comcall [rbp+rdi+TShape.Shape],IVGShape,Release
          comcall rsi,IVGShape,Release
          mov     rsi,[tmp]
          mov     [rbp+rdi+TShape.Depth],-1
        @@:
        sub edi,16
      jns .Welding
      movzx   rdx,byte[rbp+r12+TShape.Color]
      movzx   r8,byte[rbp+r12+TShape.Color+1]
      movzx   r9,byte[rbp+r12+TShape.Color+2]
      cominvk CorelApp,CreateRGBColor,rdx,r8,r9,tmp
      mov     rdi,[tmp]
      comcall rsi,IVGShape,Get_Fill,tmp
      comcall [tmp],IVGFill,Set_UniformColor,rdi
      comcall [tmp],IVGFill,Release
      comcall rdi,IVGColor,Release
      comcall rsi,IVGShape,Release
    .AlmostDone:
    sub     r12,16
  jne .WeldTiles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Error handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  jmp .quit
  .error:
    invoke MessageBoxW,0,rbx,str2GIS4CDR,0
  .quit:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Free resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke  VirtualFree,[Shapes],0,MEM_RELEASE
  invoke  DeleteFileW,HTMLFileName
  invoke  InternetCloseHandle,[hRequest]
  invoke  InternetCloseHandle,[hConnect]
  invoke  InternetCloseHandle,[hSession]
  invoke  closesocket,[Sock]
  invoke  WSACleanup
  invoke  DestroyWindow,[ProgressBar]
  invoke  DestroyWindow,[ProgressText]
  invoke  TerminateThread,[ProgressThreadHandle],0

  mov     rax,[CurveInfo]
  movdqa  xmm0,dqword[CurveData]
  movdqu  dqword[rax+SAFEARRAY.pvData],xmm0
  invoke  SafeArrayDestroy,rax

  cominvk CorelDoc,EndCommandGroup
  cominvk CorelApp,Set_Optimization,0
  cominvk CorelApp,Refresh
  cominvk Curve,Release
  cominvk LabelsLayer,Release
  cominvk GraphicsLayer,Release
  cominvk CorelDoc,Release
  movdqa  xmm6,[_xmm6]
  movdqa  xmm7,[_xmm7]
  .exit:
  xor eax,eax
ret
endp

proc OnLoad           ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  mov     [CorelApp],rdx
  comcall rdx,IVGApplication,AddRef
  ret
endp

proc StartSession     ;(const self:IVGAppPlugin):LongInt;stdcall;
  mov     eax,1
  cpuid
  test    ecx,1 shl 19 ;SSE 4.1
  je .CPUNotSupported
    cominvk CorelApp,AddPluginCommand,str2GIS4CDR,strButtonCaption,strButtonCaption,tmp
    cominvk CorelApp,AdviseEvents,IPlugin,EventsCookie
    xor     eax,eax
    ret
  .CPUNotSupported:
  invoke MessageBoxW,[CorelWndHandle],errCPUNotSupported,str2GIS4CDR,MB_TASKMODAL
  mov    eax,E_FAIL
  ret
endp

proc StopSession      ;(const self:IVGAppPlugin):LongInt;stdcall;
  cominvk CorelApp,UnadviseEvents,[EventsCookie]
  xor     eax,eax
  ret
endp

proc OnUnload         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CorelApp,Release
  xor     eax,eax
  ret
endp

varInt               VARIANT VT_I4
LayerProgress        dq strTerrainLoad,strRoadsLoad,strBuildingsLoad,strLabelsLoad
types                dq strTypeAll,0
IPlugin              dq IPluginVMT
IPluginVMT           dq QueryInterface,\
                        AddRef,\
                        Release,\
                        GetTypeInfoCount,\
                        GetTypeInfo,\
                        GetIDsOfNames,\
                        Invoke,\
                        OnLoad,\
                        StartSession,\
                        StopSession,\
                        OnUnload
align 16
CurveData            rq 2
buf                  rb 1024
sizeof.buf=$-buf
CorelWndHandle       rq 1
CorelApp             IVGApplication
CorelDoc             IVGDocument
GraphicsLayer        IVGLayer
LabelsLayer          IVGLayer
Curve                IVGCurve
CurveInfo            rq 1
ProgressText         rq 1
ProgressBar          rq 1
Sock                 rq 1
Sock2                rq 1
hSession             rq 1
hConnect             rq 1
hRequest             rq 1
tmp                  rq 1
WebSocketData        rq 1
Objects              rq 1
Shapes               rq 1
Points               rq 1
CurveElements        rq 1
VertexArray          rq 1
Polygon              rq 1
ProgressThreadHandle rq 1
EventsCookie         rd 1
Layer                rd 1
Progress             rd 1
ProgressIncrement    rd 1
ShapesCount          rd 1
VertexArrayLen       rd 1
msg                  MSG
Rect                 RECT
SHA1Result           rb 21
SHA1temp:
  .A rd 1
  .B rd 1
  .C rd 1
  .D rd 1
  .E rd 1
SHA1_W               rd 80
WebSockHeader        WebSocketHeader
HTMLFileName         rw MAX_PATH
sizeof.HTMLFileName=$-HTMLFileName
                     rw 18 ;for "2GIS.htm"