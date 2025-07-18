MAX_SHAPES = 65536 ;maximum number of glDrawArray elements, that program can process

struct TDataHeader
  Layer      rd 1
  Width      rd 1
  NumObjects rd 1
  Progress   rd 1
ends

struct TShape
  Shape IVGShape
  align 8
  Color rd 1
  Depth rd 1
ends

struct TObject
  Color rd 1
  Depth rd 1
  Start rd 1
  Count rd 1
ends

data import
  library kernel,'KERNEL32.DLL',\
          oleaut,'OLEAUT32.DLL',\
          ole,'OLE32.DLL',\
          Shell,'SHELL32.DLL',\
          wsock,'WSOCK32.DLL',\
          wininet,'WININET.DLL',\
          user,'USER32',\
          comctl,'COMCTL32.DLL'

  import kernel,\
         GetTempPathW,'GetTempPathW',\
         CreateFileW,'CreateFileW',\
         DeleteFileW,'DeleteFileW',\
         WriteFile,'WriteFile',\
         CloseHandle,'CloseHandle',\
         CreateThread,'CreateThread',\
         ExitProcess,'ExitProcess',\
         VirtualAlloc,'VirtualAlloc',\
         VirtualFree,'VirtualFree',\
         TerminateThread,'TerminateThread'

  import oleaut,\
         SafeArrayDestroy,'SafeArrayDestroy'

  import ole,\
         CLSIDFromProgID,'CLSIDFromProgID',\
         CoCreateInstance,'CoCreateInstance',\
         CoInitializeEx,'CoInitializeEx',\
         CoUninitialize,'CoUninitialize'

  import Shell,\
         ShellExecuteW,'ShellExecuteW'

  import wsock,\
         WSAStartup,'WSAStartup',\
         WSACleanup,'WSACleanup',\
         socket,'socket',\
         bind,'bind',\
         accept,'accept',\
         listen,'listen',\
         send,'send',\
         recv,'recv',\
         closesocket,'closesocket'

  import wininet,\
         InternetOpen,'InternetOpenW',\
         InternetConnect,'InternetConnectW',\
         HttpOpenRequest,'HttpOpenRequestW',\
         HttpSendRequest,'HttpSendRequestW',\
         InternetReadFile,'InternetReadFile',\
         InternetCloseHandle,'InternetCloseHandle'

  import user,\
         CreateWindowExW,'CreateWindowExW',\
         SetLayeredWindowAttributes,'SetLayeredWindowAttributes',\
         GetWindowRect,'GetWindowRect',\
         DestroyWindow,'DestroyWindow',\
         GetMessageW,'GetMessageW',\
         DispatchMessageW,'DispatchMessageW',\
         SendMessageW,'SendMessageW',\
         GetSysColor,'GetSysColor',\
         MessageBoxW,'MessageBoxW',\
         wsprintfA,'wsprintfA'

  import comctl,\
         InitCommonControls,'InitCommonControls'
end data

data export
    export 0,AttachPlugin,'AttachPlugin'
end data

data fixups
end data

PBS_SMOOTH     = 1
COLOR_3DFACE   = 15

IPPROTO_TCP    = 6
SOMAXCONN      = 5

INTERNET_OPEN_TYPE_PRECONFIG           = 0
INTERNET_SERVICE_HTTP                  = 3
INTERNET_FLAG_IGNORE_CERT_CN_INVALID   = $00001000
INTERNET_FLAG_IGNORE_CERT_DATE_INVALID = $00002000
INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS = $00004000
INTERNET_FLAG_NO_AUTH                  = $00040000
INTERNET_FLAG_KEEP_CONNECTION          = $00400000
INTERNET_FLAG_SECURE                   = $00800000

align 16
strtaVectorTilesKey     db 'taVectorTilesKey'
base64                  db 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
bswap16                 db 3,2,1,0,7,6,5,4,11,10,9,8,15,14,13,12
dbl_05                  dq 0.5,0.5
dbl_15000000            dq 15000000.0,15000000.0
LabelAspect             dq 250.0,500.0
ProgressAspect          dd 0.6,0.1,0
str2GIS4CDR             OLEstr '2GIS4CDR'
dbl_30000000            dq 30000000.0
errCPUNotSupported      du 'Процессор не поддерживается. Требуется SSE 4.1.',0
strErrNetwork           du 'Не удалось открыть 2gis.com',0
strErrKey               du 'Не удалось получить ключ для тайлового сервера 2gis',0
strErrFile              du 'Не удалось создать временный html-файл',0
strErrHTML              du 'Не удалось открыть временный html-файл',0
strErrWebSocket         du 'Соединение WebSocket разорвано',0
strInit                 du 'Инициализация',0
strChooseTiles          du 'Выберите тайлы и нажмите кнопку "Экспорт в CorelDraw"',0
strTerrainLoad          du 'Импорт геометрии ландшафта',0
strRoadsLoad            du 'Импорт геометрии дорог',0
strBuildingsLoad        du 'Импорт геометрии зданий',0
strLabelsLoad           du 'Импорт текстовых меток',0
strProcessRoads         du 'Объединение геометрии дорог',0
strSorting              du 'Упорядочивание фигур',0
strProcessTiles         du 'Объединение тайлов',0
str2gis.com             du '2gis.com',0
strGET                  du 'GET',0
strBackSlash            du '/',0
strHTTP                 du 'HTTP/1.1',0
strTypeAll              du '*/*',0
str2GIS.htm             du '2GIS.htm',0
stropen                 du 'open',0
strmsctls_progress32    du 'msctls_progress32',0
strSTATIC               du 'STATIC',0
strButtonCaption        OLEstr 'Импорт тайлов 2GIS'
strGraphics             OLEstr 'Graphics'
strLabels               OLEstr 'Labels'
strArial                OLEstr 'Arial'
ProgId                  du 'CorelDRAW.Application.20',0
fmt                     db '",Port=%i;',0
strWebSocketGUID        db '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
WebsockResponce         db 'HTTP/1.1 101 Switching Protocols',13,10,'Upgrade: websocket',13,10,'Connection: Upgrade',13,10,'Sec-WebSocket-Accept:                             ',13,10,13,10,0
HTMLBegin               db '<style>#container{width:100%;height:100%;overflow:visible}#spinner{width:30%;height:30%;position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);z-index:65535;opacity:.5;visibility:hidden}</style><svg id="spinner" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path d="M12,1A11,11,0,1,0,23,12,11,11,0,0,0,12,1Zm0,19a8,8,0,1,1,8-8A8,8,0,0,1,12,20Z" opacity=".25"/><path d="M10.14,1.16a11,11,0,0,0-9,8.92A1.59,1.59,0,0,0,2.46,12,1.52,1.52,0,0,0,4.11,10.7a8,8,0,0,1,6.66-6.61A1.42,1.42,0,0,0,12,2.69h0A1.57,1.57,0,0,0,10.14,1.16Z"><animateTransform attributeName="transform" type="rotate" dur="0.75s" values="0 12 12;360 12 12" repeatCount="indefinite"/></path></svg><div id="container"><script src="https://mapgl.2gis.com/api/js/v1"></script><script>var JakartaVectorTilesKey="'
sizeof.HTMLBegin=$-HTMLBegin
HTMLEnd                 db '</script>'
sizeof.HTMLEnd=$-HTMLEnd
JavaScript              file '2GIS4CDR_minified.js'
sizeof.JavaScript=$-JavaScript
IndexPlus1              db 1,2,0
IndexPlus2              db 2,0,1
sock_addr               sockaddr_in AF_INET,$8080,$0100007F