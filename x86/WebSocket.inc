struct WebSocketHeader
  header rw 1
  union
    struct
      tiny.mask rd 1
      tiny.data rb 8
    ends
    struct
      short.len  rw 1
      short.mask rd 1
      short.data rb 6
    ends
    struct
      long.len  rq 1
      long.mask rd 1
    ends
  ends
ends

ReadSock: ;(data: pointer; len: dword): integer;stdcall;
  xor  eax,eax
  push ebp
  push esi
  mov  ebp,[esp+12] ;data
  mov  esi,[esp+16] ;len
  jmp  .start
  @@:invoke recv,[Sock2],ebp,esi,0
     test   eax,eax
     jg .noerror
        invoke VirtualFree,[WebSocketData],0,MEM_RELEASE
        mov    ebx,strErrWebSocket
        mov    dword[esp+8],Invoke.error
        jmp    .quit
     .noerror:
     add    ebp,eax
     .start:
     sub    esi,eax
  jne @b
  mov  eax,ebp
  sub  eax,[esp+12] ;data
  .quit:
  pop  esi
  pop  ebp
ret 8

;xmm0-xmm3 - data to calc SHA1
SHA1_Calc:
  pshufb xmm0,xmm4
  pshufb xmm1,xmm4
  pshufb xmm2,xmm4
  pshufb xmm3,xmm4
  movdqu dqword[buf],xmm0
  movdqu dqword[buf+16],xmm1
  movdqu dqword[buf+32],xmm2
  movdqu dqword[buf+48],xmm3
  movdqu xmm0,dqword[SHA1Result]
  mov    eax,dword[SHA1Result+16]
  movdqu dqword[SHA1temp],xmm0
  mov    [SHA1temp.E],eax
  xor    ecx,ecx ; i
  .cycle_loop:
      mov  eax,dword[buf+ecx*4]
      cmp  ecx,16
      jb @f
         mov  eax,dword[SHA1_W+ecx*4-12] ;rol(w[j-3] ^ w[j-8] ^ w[j-14] ^ w[j-16], 1)
         xor  eax,dword[SHA1_W+ecx*4-32]
         xor  eax,dword[SHA1_W+ecx*4-56]
         xor  eax,dword[SHA1_W+ecx*4-64]
         rol  eax,1
      @@:
      mov  ebx,[SHA1temp.A]
      mov  dword[SHA1_W+ecx*4],eax
      rol  ebx,5
      add  ebx,eax
      mov  eax,ecx
      mov  edx,3435973837          ;div by 20
      mul  edx
      shr  edx,4
      mov  eax,[SHA1temp.B]
      jmp  [.case+edx*4]
      .case dd .0,.1,.2,.3
      .0:mov     edx,eax
         and     eax,[SHA1temp.C]  ;(b & c) | ((~b) & d)
         not     edx
         and     edx,[SHA1temp.D]
         or      eax,edx
         lea     ebx,[ebx+eax+$5A827999]
         jmp     .endcase
      .1:xor     eax,[SHA1temp.C]  ;b ^ c ^ d
         xor     eax,[SHA1temp.D]
         lea     ebx,[ebx+eax+$6ED9EBA1]
         jmp     .endcase
      .2:mov     edx,eax
         and     eax,[SHA1temp.C]  ;(b & c) | (b & d) | (c & d)
         and     edx,[SHA1temp.D]
         or      eax,edx
         mov     edx,[SHA1temp.C]
         and     edx,[SHA1temp.D]
         or      eax,edx
         lea     ebx,[ebx+eax+$8F1BBCDC]
         jmp     .endcase
      .3:xor     eax,[SHA1temp.C]  ;b ^ c ^ d
         xor     eax,[SHA1temp.D]
         lea     ebx,[ebx+eax+$CA62C1D6]
      .endcase:
      mov     eax,[SHA1temp.A]
      xchg    eax,[SHA1temp.B]
      rol     eax,30
      xchg    eax,[SHA1temp.C]
      xchg    eax,[SHA1temp.D]
      xchg    eax,[SHA1temp.E]
      add     ebx,eax
      mov     [SHA1temp.A],ebx    ;a = (a leftrotate 5) + f + e + k + w[i]
      inc     ecx
      cmp     ecx,80
  jne .cycle_loop
  mov     eax,[SHA1temp.A]
  mov     ebx,[SHA1temp.B]
  mov     ecx,[SHA1temp.C]
  mov     edx,[SHA1temp.D]
  mov     edi,[SHA1temp.E]
  add     dword[SHA1Result],eax
  add     dword[SHA1Result+4],ebx
  add     dword[SHA1Result+8],ecx
  add     dword[SHA1Result+12],edx
  add     dword[SHA1Result+16],edi
ret

macro SHA1Base64 lpData,dSize,lpOut{
  mov     dword[SHA1Result],$67452301
  mov     dword[SHA1Result+4],$EFCDAB89
  mov     dword[SHA1Result+8],$98BADCFE
  mov     dword[SHA1Result+12],$10325476
  mov     dword[SHA1Result+16],$C3D2E1F0
  mov     esi,lpData
  movdqa  xmm4,dqword[bswap16]
  lea     ebp,[dSize]
  push    ebp
;base data processing
  jmp .main_start
  .main_loop:
    movdqu  xmm0,[esi]
    movdqu  xmm1,[esi+16]
    movdqu  xmm2,[esi+32]
    movdqu  xmm3,[esi+48]
    call    SHA1_Calc
    add     esi,64
    .main_start:
    sub     ebp,64
  jnle .main_loop
  pop     ebx
;remaind data processing
  xor     eax,eax
  mov     edi,buf
  mov     ecx,16
  rep     stosd
  mov     edi,buf
  lea     ebp,[ebx*8]
  and     ebx,3Fh
  mov     ecx,ebx
  rep     movsb
  mov     byte[edi],80h       ;finalization bit
  movdqu  xmm0,dqword[buf]
  movdqu  xmm1,dqword[buf+16]
  movdqu  xmm2,dqword[buf+32]
  movdqu  xmm3,dqword[buf+48]
  cmp     ebx,56
  jb @f
     call  SHA1_Calc
     pxor  xmm0,xmm0
     pxor  xmm1,xmm1
     pxor  xmm2,xmm2
     pxor  xmm3,xmm3
  @@:
  bswap   ebp                 ;insert data length in bits
  pinsrd  xmm3,ebp,3
  call  SHA1_Calc

;base64 encoding
  mov    esi,SHA1Result
  movdqu xmm0,[esi]
  mov    eax,[esi+16]
  pshufb xmm0,xmm4
  bswap  eax
  movdqu [esi],xmm0
  mov    [esi+16],eax
  mov    edi,lpOut
  mov    ebp,-21
  @@:mov   eax,[esi+21+ebp]
     bswap eax
     mov   ebx,eax
     mov   ecx,eax
     mov   edx,eax
     shr   eax,26
     shr   ebx,20
     shr   ecx,14
     shr   edx,8
     and   ebx,63
     and   ecx,63
     and   edx,63
     mov   al,byte[base64+eax]
     mov   ah,byte[base64+ebx]
     stosw
     mov   al,byte[base64+ecx]
     mov   ah,byte[base64+edx]
     stosw
     add   ebp,3
  jne @b
  mov byte[edi-1],'='
}