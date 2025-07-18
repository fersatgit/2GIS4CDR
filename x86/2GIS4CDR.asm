format PE GUI 4.0 DLL as 'cpg'
entry DllEntryPoint

include 'encoding\win1251.inc'
include 'win32w.inc'
include 'CorelDraw.inc'
include '..\API.inc'
include 'WebSocket.inc'

DllEntryPoint: ;hinstDLL,fdwReason,lpvReserved
  mov eax,TRUE
ret 12

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov eax,[esp+4]
  mov dword[eax],IPlugin
  mov eax,256
ret 4

ProgressThread: ;(Parent: dword);stdcall
  invoke   InitCommonControls
  invoke   GetWindowRect,dword[esp+8],Rect
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
  invoke   CreateWindowExW,0,strmsctls_progress32,0,WS_VISIBLE+WS_POPUP+WS_CHILD+PBS_SMOOTH,[Rect.left],[Rect.top],[Rect.right],[Rect.bottom],dword[esp+16],0,0,0
  mov      [ProgressBar],eax
  invoke   SendMessageW,eax,PBM_SETRANGE,0,1024 shl 16
  mov      eax,[Rect.bottom]
  sub      eax,20
  shr      eax,1
  add      eax,[Rect.top]
  invoke   CreateWindowExW,WS_EX_LAYERED,strSTATIC,strInit,WS_VISIBLE+SS_CENTER+WS_POPUP,[Rect.left],eax,[Rect.right],20,[ProgressBar],0,0,0
  mov      [ProgressText],eax
  invoke   GetSysColor,COLOR_3DFACE
  invoke   SetLayeredWindowAttributes,[ProgressText],eax,0,LWA_COLORKEY
  @@:invoke GetMessageW,msg,0,0,0
     invoke DispatchMessageW,msg
  jmp @b

QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov eax,[esp+12]
  mov dword[eax],IPlugin
  xor eax,eax
ret 12
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret 4
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 8
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 12
GetIDsOfNames:    ;(const self:IVGAppPlugin; const IID: TGUID; Names: Pointer;NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 24

Invoke:           ;(const self:IVGAppPlugin; DispID: Integer; const IID: TGUID; LocaleID: Integer;Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  mov eax,[esp+8]
  cmp eax,OnPluginCommand
  je .OnPluginCommand
    cmp eax,OnUpdatePluginCommand
    jne .exit
      mov      edx,[esp+24]
      mov      edx,[edx+DISPPARAMS.rgvarg]
      mov      eax,dword[edx+sizeof.VARIANT*2+VARIANT.data]
      movdqu   xmm0,[eax]
      pcmpeqb  xmm0,dqword[str2GIS4CDR]
      pmovmskb eax,xmm0
      inc      ax
      jne    .exit
        push    ebx
        mov     ebx,dword[edx+sizeof.VARIANT*1+VARIANT.data]
        mov     [tmp],0
        cominvk CorelApp,Get_ActiveDocument,tmp
        mov     word[ebx],0
        cmp     [tmp],0
        je @f
          mov     word[ebx],1
          comcall [tmp],IVGDocument,Release
        @@:
        xor eax,eax
        pop ebx
        ret 36

 .OnPluginCommand:
  mov      eax,[esp+24]
  mov      eax,[eax+DISPPARAMS.rgvarg]
  mov      eax,dword[eax+VARIANT.data]
  movdqu   xmm0,[eax]
  pcmpeqb  xmm0,dqword[str2GIS4CDR]
  pmovmskb eax,xmm0
  inc      ax
  jne .exit
  pushad
  cominvk CorelApp,Set_Optimization,1
  cominvk CorelApp,Get_ActiveWindow,tmp
  mov     ebx,[tmp]
  comcall ebx,IVGWindow,Get_Handle,CorelWndHandle
  comcall ebx,IVGWindow,Release
  cominvk CorelApp,Get_ActiveDocument,CorelDoc
  cominvk CorelDoc,Set_Unit,cdrTenthMicron
  cominvk CorelDoc,BeginCommandGroup,str2GIS4CDR
  cominvk CorelApp,CreateCurve,[CorelDoc],Curve
  cominvk Curve,GetCurveInfo,CurveInfo
  mov     eax,[CurveInfo]
  movdqu  xmm0,dqword[eax+SAFEARRAY.cLocks]
  movdqa  dqword[CurveData],xmm0
  cominvk CorelDoc,Get_ActivePage,tmp
  mov     esi,[tmp]
  comcall esi,IVGPage,Get_Layers,tmp
  mov     edi,[tmp]
  mov     [GraphicsLayer],0
  mov     [LabelsLayer],0
  comcall edi,IVGLayers,Find,strGraphics,GraphicsLayer
  comcall edi,IVGLayers,Find,strLabels,LabelsLayer
  comcall edi,IVGLayers,Release
  cmp     [GraphicsLayer],0
  jne @f
    comcall esi,IVGPage,CreateLayer,strGraphics,GraphicsLayer
  @@:
  cmp     [LabelsLayer],0
  jne @f
    comcall esi,IVGPage,CreateLayer,strLabels,LabelsLayer
  @@:
  comcall esi,IVGPage,Release
  invoke CreateThread,0,0,ProgressThread,[CorelWndHandle],0,0
  mov    [ProgressThreadHandle],eax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Initializing WebSocket server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke WSAStartup,$0202,buf
  invoke socket,AF_INET,SOCK_STREAM,IPPROTO_TCP
  mov    [Sock],eax
  mov    [sock_addr.sin_port],$8080
  @@:inc    [sock_addr.sin_port]
     invoke bind,[Sock],sock_addr,sizeof.sockaddr_in
     test   eax,eax
  jne @b
  invoke listen,[Sock],SOMAXCONN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Generating html-file with public api-key and execute it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  mov    ebx,strErrNetwork
  invoke InternetOpen,0,INTERNET_OPEN_TYPE_PRECONFIG,0,0,0
  test   eax,eax
  je     .error
  mov    [hSession],eax
  invoke InternetConnect,eax,str2gis.com,443,0,0,INTERNET_SERVICE_HTTP,0,0
  test   eax,eax
  je     .error
  mov    [hConnect],eax
  invoke HttpOpenRequest,[hConnect],strGET,strBackSlash,strHTTP,0,types,INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS+INTERNET_FLAG_IGNORE_CERT_CN_INVALID+INTERNET_FLAG_IGNORE_CERT_DATE_INVALID+INTERNET_FLAG_NO_AUTH+INTERNET_FLAG_KEEP_CONNECTION+INTERNET_FLAG_SECURE,0
  test   eax,eax
  je     .error
  mov    [hRequest],eax
  invoke HttpSendRequest,eax,0,0,0,0
  test   eax,eax
  je     .error
  mov    [tmp],4
  mov    ebx,strErrKey
  .ReadMainPage:
     invoke InternetReadFile,[hRequest],buf+16,sizeof.buf-16,tmp
     cmp    [tmp],0
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
     mov      eax,[tmp]
     movdqu   xmm0,dqword[buf+eax-16]
     movdqa   dqword[buf],xmm0
  jne .ReadMainPage
  lea    esi,[buf+edx+18]
  lea    ecx,[buf+eax]
  mov    edi,buf
  sub    ecx,esi
  rep    movsb
  lea    esi,[edx+18]
  invoke InternetReadFile,[hRequest],edi,esi,tmp
  xor    esi,esi
  @@:inc esi
     cmp [buf+esi],'"'
  jne @b
  mov    ebx,strErrFile
  invoke GetTempPathW,sizeof.HTMLFileName,HTMLFileName
  movdqu xmm0,dqword[str2GIS.htm]
  movdqu dqword[HTMLFileName+eax*2],xmm0
  mov    [HTMLFileName+eax*2+16],0
  invoke CreateFileW,HTMLFileName,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0
  cmp    eax,INVALID_HANDLE_VALUE
  je     .error;
  mov    ebx,eax
  invoke WriteFile,eax,HTMLBegin,sizeof.HTMLBegin,tmp,0
  invoke WriteFile,ebx,buf,esi,tmp,0
  movzx  eax,[sock_addr.sin_port]
  rol    ax,8
  invoke wsprintfA,buf,fmt,eax
  invoke WriteFile,ebx,buf,eax,tmp,0
  invoke WriteFile,ebx,JavaScript,sizeof.JavaScript,tmp,0
  invoke WriteFile,ebx,HTMLEnd,sizeof.HTMLEnd,tmp,0
  invoke CloseHandle,ebx
  mov    ebx,strErrHTML
  invoke ShellExecuteW,0,stropen,HTMLFileName,0,0,SW_SHOW
  cmp    eax,33
  jl     .error
  invoke SendMessageW,[ProgressText],WM_SETTEXT,0,strChooseTiles

  invoke VirtualAlloc,0,MAX_SHAPES*sizeof.TShape,MEM_COMMIT,PAGE_READWRITE
  mov    [Shapes],eax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Websocket handshake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  mov    [tmp],sizeof.sockaddr_in
  invoke accept,[Sock],sock_addr,tmp
  mov    [Sock2],eax
  invoke recv,eax,buf,sizeof.buf,0
  mov    edi,buf+4
  xor    eax,eax
  @@:inc edi
     cmp dword[edi-4],'ey: '
  jne @b
  @@:inc eax
     cmp byte[edi+eax],13
  jne @b
  movdqu xmm0,dqword[strWebSocketGUID]
  movdqu xmm1,dqword[strWebSocketGUID+16]
  mov    edx,dword[strWebSocketGUID+32]
  movdqu [edi+eax],xmm0
  movdqu [edi+eax+16],xmm1
  mov    [edi+eax+32],edx
  SHA1Base64 edi,eax+36,WebsockResponce+97
  invoke send,[Sock2],WebsockResponce,129,0

  mov [ShapesCount],0
  mov [ProgressFloat],0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Websocket data receiving loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  .Main:
    xor edi,edi
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
      mov     esi,[WebSockHeader.tiny.mask]
      cmp     ebx,126
      jne @f
        stdcall ReadSock,WebSockHeader+6,2
        movzx   ebx,[WebSockHeader.short.len]
        rol     bx,8
        mov     esi,[WebSockHeader.short.mask]
      @@:
      cmp     ebx,127
      jne @f
        stdcall ReadSock,WebSockHeader+6,8
        mov     ebx,dword[WebSockHeader.long.len+4]
        bswap   ebx
        mov     esi,[WebSockHeader.long.mask]
      @@:
      lea    edx,[ebx+edi]
      invoke VirtualAlloc,0,edx,MEM_COMMIT,PAGE_READWRITE
      mov    [tmp],eax
      test   edi,edi
      je @f
        pushad
        mov    ecx,edi
        mov    esi,[WebSocketData]
        mov    edi,[tmp]
        rep    movsb
        popad
        invoke VirtualFree,[WebSocketData],0,MEM_RELEASE
      @@:
      mov     ebp,[tmp]
      mov     [WebSocketData],ebp
      add     ebp,edi
      stdcall ReadSock,ebp,ebx
      lea     ecx,[ebx+3]
      shr     ecx,2
      @@:xor [ebp+ecx*4-4],esi
      loop @b
      add     edi,ebx
      test    [WebSockHeader.header],$80
    je .WebSocketRead

    mov      ebp,[WebSocketData]
    mov      ebx,[ebp+TDataHeader.Layer]
    mov      [Layer],ebx
    invoke   SendMessageW,[ProgressText],WM_SETTEXT,0,[LayerProgress+ebx*4]
    movsd    xmm7,[dbl_30000000]
    cvtsi2sd xmm0,[ebp+TDataHeader.Width]
    divsd    xmm7,xmm0
    shufpd   xmm7,xmm7,0
    movapd   dqword[Scale],xmm7
    mov      eax,[ebp+TDataHeader.NumObjects]
    mov      edx,[ebp+TDataHeader.Progress]
    mov      [NumObjects],eax
    mov      [Progress],edx
    cvtsi2ss xmm0,edx
    cvtsi2ss xmm1,eax
    subss    xmm0,[ProgressFloat]
    divss    xmm0,xmm1
    movss    [ProgressIncrement],xmm0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Labels processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    cmp ebx,3
    jne .Graphics
      lea   ebx,[ebp+sizeof.TDataHeader+eax*8]
      add   ebp,sizeof.TDataHeader
      @@:mov      edi,[ebx] ;string length
         shl      dword[ebx],1
         mov      eax,16
         lea      edx,[edi+15]
         cmp      eax,edi
         cmova    eax,edi
         shr      edx,4
         movd     xmm0,eax
         pinsrd   xmm0,edx,1
         movq     xmm1,[ebp]
         cvtdq2pd xmm0,xmm0
         cvtdq2pd xmm1,xmm1
         mulpd    xmm0,dqword[Scale]
         mulpd    xmm1,dqword[Scale]
         mulpd    xmm0,dqword[LabelAspect]
         movapd   xmm2,xmm0
         mulpd    xmm2,dqword[dbl_05]
         subpd    xmm1,xmm2
         subpd    xmm1,dqword[dbl_15000000]
         addpd    xmm0,xmm1

         add      ebx,4
         push     tmp
         push     cdrCenterAlignment
         push     0
         push     0
         push     0
         push     12
         push     strArial
         push     0
         push     0
         push     ebx
         sub      esp,32
         movupd   [esp],xmm1
         movupd   [esp+16],xmm0
         cominvk  LabelsLayer,CreateParagraphText
         mov      esi,[tmp]
         comcall  esi,IVGShape,Get_Text,tmp
         comcall  esi,IVGShape,Release
         mov      esi,[tmp]
         comcall  esi,IVGText,FitTextToFrame
         comcall  esi,IVGText,Release

         movss    xmm0,[ProgressFloat]
         cvtss2si eax,xmm0
         invoke   SendMessageW,[ProgressBar],PBM_SETPOS,eax,0
         movss    xmm0,[ProgressFloat]
         addss    xmm0,[ProgressIncrement]
         movss    [ProgressFloat],xmm0
         add      ebp,8
         lea      ebx,[ebx+edi*2]
         dec      [NumObjects]
      jne @b
      jmp .NextObject
    .Graphics:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Graphics processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    shl      eax,4    ;size of Objects
    sub      edi,sizeof.TDataHeader
    sub      edi,eax
    shr      edi,3
    lea      edx,[ebp+sizeof.TDataHeader]
    mov      [Objects],edx
    add      edx,eax
    shl      edi,5
    mov      [Points],edx
    lea      edx,[edi+edi*2]
    invoke   VirtualAlloc,0,edx,MEM_COMMIT,PAGE_READWRITE
    mov      edx,[CurveInfo]
    mov      [CurveElements],eax
    mov      [edx+SAFEARRAY.pvData],eax
    lea      eax,[eax+edi*2]
    mov      [VertexArray],eax
    shr      edi,2
    add      eax,edi
    mov      [Polygon],eax

    ;Scaling graphics to fit 3x3m area
    mov      eax,[Points]
    movdqa   xmm2,dqword[dbl_15000000]
    @@:cvtpi2pd xmm0,[eax+edi-8]
       mulpd    xmm0,dqword[Scale]
       subpd    xmm0,xmm2
       cvtpd2dq xmm0,xmm0
       movq     [eax+edi-8],xmm0
       sub      edi,8
    jne @b

    dec      [NumObjects]
    .ObjectsProcessing:
       mov  [CurveElementsLen],0
       mov  eax,[NumObjects]
       shl  eax,4
       add  eax,[Objects]
       mov  esi,[eax+TObject.Start]
       shl  esi,3
       add  esi,[Points]
       mov  ecx,[eax+TObject.Count]
       mov  edi,[VertexArray]
       mov  [VertexArrayLen],ecx
       add  ecx,ecx
       rep  movsd

       mov  edi,[Polygon]
       mov  esi,[VertexArray]
       .VertexArray2CorelCurve:
         ;Merging triangles into a polygon
         sub      [VertexArrayLen],3
         mov      [PolyLen],4
         mov      ebx,[VertexArrayLen]
         movdqu   xmm0,[esi+ebx*8]
         movq     xmm1,[esi+ebx*8+16]
         movdqu   [edi],xmm0
         movq     [edi+16],xmm1
         movq     [edi+24],xmm0
         jmp      .StartMerge
         .Merge:
           mov      ebp,2
           .Tri1:
             mov      ecx,[PolyLen]
             lea      ecx,[ecx*8-8]
             .Tri2:
               lea      edx,[ebx+ebp]
               movzx    eax,[IndexPlus1+ebp]
               movdqu   xmm0,[edi+ecx-8]
               add      eax,ebx
               movdqu   xmm1,[esi+edx*8-8]
               movq     xmm2,[esi+eax*8]
               movsd    xmm1,xmm2
               pcmpeqd  xmm0,xmm1
               pmovmskb eax,xmm0
               inc      ax
               jne .NextTriangle
                 mov    eax,[PolyLen]
                 lea    eax,[eax*8-8]
                 mov    edx,[Polygon]
                 add    edx,ecx
                 sub    eax,ecx
                 @@:movq xmm0,[edx+eax]
                    movq [edx+eax+8],xmm0
                    sub  eax,8
                 jns @b
                 movzx   eax,[IndexPlus2+ebp]
                 sub    [VertexArrayLen],3
                 add    eax,ebx
                 inc    [PolyLen]
                 movq   xmm0,[esi+eax*8]
                 mov    eax,[VertexArrayLen]
                 movq   [edx],xmm0
                 movdqu xmm0,[esi+eax*8]
                 movq   xmm1,[esi+eax*8+16]
                 movdqu [esi+ebx*8],xmm0
                 movq   [esi+ebx*8+16],xmm1
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
           cmp [PolyLen],5
           jl .continue
         @@:

         ;Removing bridges from polygons with holes
         mov edi,[Polygon]
         .RemoveBridges:
           mov ebx,[PolyLen]
           dec ebx                
           mov eax,ebx
           .Right:
             xor edx,edx
             .Left:mov ecx,eax
                   sub ecx,ebx
                   or  ecx,edx
                   je .skip
                     movq     xmm0,[edi+eax*8]
                     movq     xmm1,[edi+edx*8]
                     pcmpeqd  xmm0,xmm1
                     pmovmskb ecx,xmm0
                     inc      cx
                     jne .skip
                       lea ecx,[ebx+1]
                       sub ecx,eax
                       mov ebp,edi
                       @@:movq xmm0,[ebp+eax*8]
                          movq [ebp+edx*8],xmm0
                          add  ebp,8
                       loop @b
                       sub edx,eax
                       add [PolyLen],edx
                       jmp .RemoveBridges
                   .skip:
                   inc  edx
                   cmp  eax,edx
             jne .Left
             dec eax
           jne .Right

         ;Generating data for PutCurveInfo
         lea edx,[ebx+1]
         mov ebp,[CurveElementsLen]
         add [CurveElementsLen],edx
         shl ebp,5
         shl edx,5
         add ebp,[CurveElements]
         shl ebx,3
         @@:cvtpi2pd xmm0,[edi+ebx]
             movupd  dqword[ebp+ebx*4+CurveElement.PositionX],xmm0
             mov     [ebp+ebx*4+CurveElement.ElementType],cdrElementLine
             mov     [ebp+ebx*4+CurveElement.NodeType],cdrCuspNode
             mov     [ebp+ebx*4+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             sub     ebx,8
         jns @b
         mov [ebp+CurveElement.ElementType],cdrElementStart
         mov [ebp+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
         mov [ebp+edx+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed

         .continue:
         cmp  [VertexArrayLen],0
       jg .VertexArray2CorelCurve

       ;Creating shape and add it to list
       mov      eax,[CurveInfo]
       mov      edx,[CurveElementsLen]
       mov      [eax+SAFEARRAY.rgsabound.cElements],edx
       cominvk  Curve,PutCurveInfo,CurveInfo,edx,tmp
       mov      ebx,[ShapesCount]
       shl      ebx,4
       add      ebx,[Shapes]
       lea      edx,[ebx+TShape.Shape]
       cominvk  GraphicsLayer,CreateCurve,[Curve],edx
       mov      edx,[NumObjects]
       shl      edx,4
       add      edx,[Objects]
       movq     xmm0,qword[edx+TObject.Color]
       movq     qword[ebx+TShape.Color],xmm0

       ;For roads do break apart with weld
       cmp      [Layer],1
       jne .NoRoad
         invoke  SendMessageW,[ProgressText],WM_SETTEXT,0,strProcessRoads
         comcall [ebx+TShape.Shape],IVGShape,BreakApartEx,tmp
         comcall [ebx+TShape.Shape],IVGShape,Release
         mov     esi,[tmp]
         comcall esi,IVGShapeRange,Get_Count,tmp
         mov     edi,[tmp]
         comcall esi,IVGShapeRange,Get_Item,VT_I4,0,edi,0,tmp
         mov     ebp,[tmp]
         dec     edi
         jle .SimpleShape
           @@:comcall esi,IVGShapeRange,Get_Item,VT_I4,0,edi,0,tmp
              push    eax
              comcall ebp,IVGShape,Weld,[tmp],0,0,esp
              comcall [tmp],IVGShape,Release
              comcall ebp,IVGShape,Release
              pop     ebp
              dec     edi
           jne @b
         .SimpleShape:
         mov     [ebx+TShape.Shape],ebp
         comcall esi,IVGShapeRange,Release
       .NoRoad:

       inc      [ShapesCount]
       movss    xmm7,[ProgressFloat]
       cvtss2si eax,xmm7
       invoke   SendMessageW,[ProgressBar],PBM_SETPOS,eax,0
       movss    xmm7,[ProgressFloat]
       addss    xmm7,[ProgressIncrement]
       movss    [ProgressFloat],xmm7

       ;if Shapes array is overfilled just skip remeining data
       cmp      [ShapesCount],MAX_SHAPES
       jne @f
         invoke VirtualFree,[CurveElements],0,MEM_RELEASE
         invoke VirtualFree,[WebSocketData],0,MEM_RELEASE
         invoke SendMessageW,[ProgressBar],PBM_SETPOS,1024,0
         jmp    .CloseSock
       @@:

       dec      [NumObjects]
    jns .ObjectsProcessing

    .NextObject:
    cvtsi2ss xmm0,[Progress]
    movss    [ProgressFloat],xmm0
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
  mov    ebp,[Shapes]
  sub    edi,16
  .Sort:
    mov ebx,edi
    mov eax,[ebp+edi+TShape.Depth]
    lea edx,[edi-16]
    .Sort2:
      cmp eax,[ebp+edx+TShape.Depth]
      jl @f
        mov ebx,edx
        mov eax,[ebp+edx+TShape.Depth]
      @@:
      sub edx,16
    jns .Sort2
    movdqa  xmm0,[ebp+ebx]
    movdqa  xmm1,[ebp+edi]
    movdqa  [ebp+edi],xmm0
    movdqa  [ebp+ebx],xmm1
    comcall [ebp+edi+TShape.Shape],IVGShape,OrderToFront
    sub     edi,16
  jne .Sort
  comcall [ebp+edi+TShape.Shape],IVGShape,OrderToFront

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Welding tiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke SendMessageW,[ProgressText],WM_SETTEXT,0,strProcessTiles
  mov    ebx,[ShapesCount]
  shl    ebx,4
  sub    ebx,16
  .WeldTiles:
    mov     esi,[ebp+ebx+TShape.Shape]
    cmp     [ebp+ebx+TShape.Depth],-1
    je .AlmostDone
      lea edi,[ebx-16]
      .Welding:
        mov edx,[ebp+ebx+TShape.Color]
        mov eax,[ebp+ebx+TShape.Depth]
        sub edx,[ebp+edi+TShape.Color]
        sub eax,[ebp+edi+TShape.Depth]
        or  eax,edx                    ;if color and depth are equal - weld shapes
        jne @f
          comcall esi,IVGShape,Weld,[ebp+edi+TShape.Shape],0,0,tmp
          comcall [ebp+edi+TShape.Shape],IVGShape,Release
          comcall esi,IVGShape,Release
          mov     esi,[tmp]
          mov     [ebp+edi+TShape.Depth],-1
        @@:
        sub edi,16
      jns .Welding
      movzx   eax,byte[ebp+ebx+TShape.Color]
      movzx   ecx,byte[ebp+ebx+TShape.Color+1]
      movzx   edx,byte[ebp+ebx+TShape.Color+2]
      cominvk CorelApp,CreateRGBColor,eax,ecx,edx,tmp
      mov     edi,[tmp]
      comcall esi,IVGShape,Get_Fill,tmp
      comcall [tmp],IVGFill,Set_UniformColor,edi
      comcall [tmp],IVGFill,Release
      comcall edi,IVGColor,Release
      comcall esi,IVGShape,Release
    .AlmostDone:
    sub     ebx,16
  jne .WeldTiles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Error handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  jmp .quit
  .error:
    invoke MessageBoxW,0,ebx,str2GIS4CDR,0
  .quit:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Free resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  invoke VirtualFree,[Shapes],0,MEM_RELEASE
  invoke DeleteFileW,HTMLFileName
  invoke InternetCloseHandle,[hRequest]
  invoke InternetCloseHandle,[hConnect]
  invoke InternetCloseHandle,[hSession]
  invoke closesocket,[Sock]
  invoke WSACleanup
  invoke DestroyWindow,[ProgressBar]
  invoke DestroyWindow,[ProgressText]
  invoke TerminateThread,[ProgressThreadHandle],0

  mov     eax,[CurveInfo]
  movdqa  xmm0,dqword[CurveData]
  movdqu  dqword[eax+SAFEARRAY.cLocks],xmm0
  invoke  SafeArrayDestroy,eax

  cominvk CorelDoc,EndCommandGroup
  cominvk CorelApp,Set_Optimization,0
  cominvk CorelApp,Refresh
  cominvk Curve,Release
  cominvk LabelsLayer,Release
  cominvk GraphicsLayer,Release
  cominvk CorelDoc,Release
  popad
  .exit:
  xor eax,eax
ret 36

OnLoad:           ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  mov     eax,[esp+8]
  mov     [CorelApp],eax
  comcall eax,IVGApplication,AddRef
ret 8

StartSession:     ;(const self:IVGAppPlugin):LongInt;stdcall;
  mov     eax,1
  cpuid
  test    ecx,1 shl 19 ;SSE 4.1
  je .CPUNotSupported
    cominvk CorelApp,AddPluginCommand,str2GIS4CDR,strButtonCaption,strButtonCaption,tmp
    cominvk CorelApp,AdviseEvents,IPlugin,EventsCookie
    xor     eax,eax
    ret 4
  .CPUNotSupported:
  invoke MessageBoxW,[CorelWndHandle],errCPUNotSupported,str2GIS4CDR,MB_TASKMODAL
  mov    eax,E_FAIL
ret 4

StopSession:      ;(const self:IVGAppPlugin):LongInt;stdcall;
  cominvk CorelApp,UnadviseEvents,[EventsCookie]
  xor     eax,eax
ret 4

OnUnload:         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CorelApp,Release
  xor     eax,eax
ret 4

LayerProgress        dd strTerrainLoad,strRoadsLoad,strBuildingsLoad,strLabelsLoad
types                dd strTypeAll,0
IPlugin              dd IPluginVMT
IPluginVMT           dd QueryInterface,\
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
Scale                rq 2
CurveData            rq 2
buf                  rb 1024
sizeof.buf=$-buf
CorelWndHandle       rd 1
CorelApp             IVGApplication
CorelDoc             IVGDocument
GraphicsLayer        IVGLayer
LabelsLayer          IVGLayer
Curve                IVGCurve
CurveInfo            rd 1
ProgressText         rd 1
ProgressBar          rd 1
Sock                 rd 1
Sock2                rd 1
hSession             rd 1
hConnect             rd 1
hRequest             rd 1
tmp                  rd 1
WebSocketData        rd 1
Objects              rd 1
Shapes               rd 1
Points               rd 1
CurveElements        rd 1
VertexArray          rd 1
Polygon              rd 1
ProgressThreadHandle rd 1
EventsCookie         rd 1
Layer                rd 1
NumObjects           rd 1
Progress             rd 1
ProgressFloat        rd 1
ProgressIncrement    rd 1
ShapesCount          rd 1
VertexArrayLen       rd 1
CurveElementsLen     rd 1
PolyLen              rd 1
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