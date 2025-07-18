var EARTH_RADIUS = 0x100000000 / Math.PI * 0.5;
var Tiles = [],Socket;
var Rad2Grad=180.0/Math.PI, Grad2Rad=Math.PI/180.0;
var Global2Lng=Rad2Grad/EARTH_RADIUS, Lng2Global=EARTH_RADIUS*Grad2Rad;

function s8(x){
  return x>=128?x-256:x;
}

function ExportGraphics(Layer,Socket,Children,Symbol,Sink,Attr,x,y,width,Progress){
    var Child,Attributes,Elements=[],Widths= [],i,j,k,n;    

    for (i = 0; i < Children.length; i++){
        Child = Children[i];
        if ((Child.symbol==Symbol)&&(Child.sink==Sink)&&(!Child.attributes.floorId)){
            Attributes=Child.vao._attributes[Attr];
            if(Child.renderingProperties.color){
              let color=Child.renderingProperties.color.value;
              Elements.push((color[3]<<24)+(color[2]<<16)+(color[1]<<8)+color[0]);
            }
            else
              Elements.push(0);
            Elements.push(Child.attributes.layerId);
            Elements.push(Child.start);
            Elements.push(Child.count);
            Widths.push(Child.renderingProperties.widthResolved+1);
        }
    }

    if (Attributes){
        const Buffer = Attributes._buffer;
        const gl = Buffer._glContext;
        var   Stride = Attributes.options.stride>>1;
        var   retrievedData = new Uint16Array(Buffer.byteLength / Uint16Array.BYTES_PER_ELEMENT);
        var   Objects=new Uint32Array(Elements.length+4+Buffer.byteLength/Stride);
        gl.bindBuffer(gl.ARRAY_BUFFER, Buffer._glBuffer);
        gl.getBufferSubData(gl.ARRAY_BUFFER, 0, retrievedData);
       
        Objects[0]=Layer;
        Objects[1]=width;
        Objects[2]=Elements.length>>2;
        Objects[3]=Progress;
        Objects.set(Elements,4);
        n = Elements.length+4;
        if(Symbol=="line")
        {
          let j=0;
          for(i=0;i<Widths.length;i++){
            let end=(Elements[j+2]+Elements[j+3])*Stride;
            let scale=Widths[i]/width*16384;
            for (k = Elements[j+2]*Stride; k<end; k+=Stride){
              Objects[n]=retrievedData[k]*4+x+s8(retrievedData[k+2] & 255)*scale;
              Objects[n+1]=retrievedData[k+1]*4+y+s8(retrievedData[k+2] >> 8)*scale;
              n+=2;
            }
            j+=4;
          }
        }
        else
          for (i = 0; i < retrievedData.length; i+=Stride){
            Objects[n]=retrievedData[i]*4+x;
            Objects[n+1]=retrievedData[i+1]*4+y;
            n+=2;
          }
        Socket.send(Objects);
    }
}

function WebSocketError(e){
  alert('Нет связи с CorelDraw');
  window.close();
}

async function onExport(){
var Encoder=new TextEncoder(),xmlHttp = new XMLHttpRequest(),page,coords,names,objcount,MoreTiles,MinX,MinY,MaxX,MaxY,Labels,LabelsView,i,j,k,n,m,t,name,OpCount=0,CurOp;
    document.getElementById("spinner").style.visibility="visible";
    MinX=MinY=Infinity;
    MaxX=MaxY=-Infinity;
    for (i = 0; i < Tiles.length; i++){
        const Tile = Tiles[i];
        if (Tile.Selected){
          MinX=Math.min(MinX,Tile.x);
          MinY=Math.min(MinY,Tile.y);
          MaxX=Math.max(MaxX,Tile.x+131072);
          MaxY=Math.max(MaxY,Tile.y+131072);
          OpCount+=10/1024;
        }
    }
    CurOp=0;
    do{
        MoreTiles = false;
        let Width=Math.max(MaxX-MinX,MaxY-MinY);
        for (i = 0; i < Tiles.length; i++){
            let Tile = Tiles[i];
            if (Tile.Selected){
                MoreTiles = true;
                map.setCenter([(Tile.x + 65536)*Global2Lng,Math.atan(Math.sinh((Tile.y + 65536) / EARTH_RADIUS))*Rad2Grad],{animate: false});
                map.setZoom(20,{animate: false});
                await map._defaultSource._impl.zenithSource.fetchTile([Math.floor((Tile.x+0x80000000)/131072),Math.floor((Tile.y+0x80000000)/131072),15,16],map._impl.state)
                const Objects = map._impl.modules.tileManager.objects,
                      OfsX=Tile.x-MinX-65536,
                      OfsY=Tile.y-MinY-65536;
                for (j = 0; j < Objects.length; j++){
                    let Obj = Objects[j];
                    if ((Obj.purpose == "zenith") && (Obj.coords[0] * 131072 - 0x80000000 == Tile.x) && (Obj.coords[1] * 131072 - 0x80000000 == Tile.y) && (Obj.coords[2] == 15)){
                        CurOp++;
                        await ExportGraphics(0,Socket,Obj.children,"polygon","fill","a_vec2_vertex",OfsX,OfsY,Width,CurOp/OpCount);
                        CurOp++;
                        let Obj2=Objects[j+1];
                        if (Obj2 && (Obj2.purpose == "zenith") && (Obj2.coords[0] * 131072 - 0x80000000 == Tile.x) && (Obj2.coords[1] * 131072 - 0x80000000 == Tile.y)&& (Obj2.coords[2] == 15))
                          await ExportGraphics(1,Socket,Obj2.children,"line","solid","a_vec2_vertex",OfsX,OfsY,Width,CurOp/OpCount);
                        CurOp++;
                        await ExportGraphics(1,Socket,Obj.children,"line","solid","a_vec2_vertex",OfsX,OfsY,Width,CurOp/OpCount);
                        CurOp++;
                        await ExportGraphics(2,Socket,Obj.children,"polygonExtrusion","topFill","a_vec4_vertex",OfsX,OfsY,Width,CurOp/OpCount);
                        CurOp+=6;

                        page=1;
                        coords=[];
                        names="";
                        objcount=0;
                        do{
                          xmlHttp.open("GET","https://catalog.api.2gis.com/3.0/items/geocode?type=building,street&locale=ru_RU&page_size=50&page="+page+"&fields=items.address,items.point&point1="+
                         Tile.x*Global2Lng+","+Math.atan(Math.sinh((Tile.y+131072) / EARTH_RADIUS))*Rad2Grad+"&point2="+
                         (Tile.x+131072)*Global2Lng+","+Math.atan(Math.sinh((Tile.y) / EARTH_RADIUS))*Rad2Grad+"&key=demo",false);
                          xmlHttp.send(null);
                          var response=xmlHttp.responseText;
                          if(response.includes('result')){
                            response=JSON.parse(response).result.items;
                            for(k=0;k<response.length;k++){
                              if((response[k].address)&&(response[k].address.components)&&(response[k].address.components[0].number))
                                name=response[k].address.components[0].number;
                              else
                                name=response[k].name;
                              names=names.concat(name);
                              coords.push(response[k].point.lon*Lng2Global-MinX,Math.asinh(Math.tan(response[k].point.lat*Grad2Rad))*EARTH_RADIUS-MinY,name.length);
                              objcount++;
                            }
                          page++;
                          }else
                            break;
                        }while(true);
                        if(objcount){
                          Labels=new Uint32Array(objcount*3+4+(names.length+1)/2);
                          LabelsView=new Uint16Array(Labels.buffer,objcount*8+16);
                          Labels[0]=3;
                          Labels[1]=Width;
                          Labels[2]=objcount;
                          Labels[3]=CurOp/OpCount;
                          n=t=0;
                          for(k=0;k<objcount;k++){
                            Labels[k*2+4]=coords[k*3];
                            Labels[k*2+5]=coords[k*3+1];
                            LabelsView[n]=coords[k*3+2];
                            n+=2;
                            for(m=0;m<coords[k*3+2];m++){
                              LabelsView[n]=names.charCodeAt(t);
                              t++;
                              n++;
                            }
                          }
                          Socket.send(Labels);
                        }

                        Tile.Selected = false;
                        console.log(Obj);
                        break;
                    }
                }
            }
        }
    } while (MoreTiles);    
    while(Socket.bufferedAmount>0)
      await new Promise(r => setTimeout(r, 500));
    window.close();    
}

function onTileClick(e){
    let point = map._impl.modules.camera.flat.unproject(e.point);
    point[0] = Math.floor(point[0] / 131072) * 131072;
    point[1] = Math.floor(point[1] / 131072) * 131072;
    const elem = Tiles.find((element) => (element.x == point[0]) && (element.y == point[1]));
    let polygon;
    if (elem.Selected = !elem.Selected)
        polygon = new mapgl.Polygon(map, {coordinates: elem.polygon._impl.options.coordinates,
                                          color: '#00009950',
                                          strokeWidth: 0});
    else
        polygon = new mapgl.Polygon(map, {coordinates: elem.polygon._impl.options.coordinates,
                                          color: '#FFFFFF00',
                                          strokeWidth: 3,
                                          strokeColor: '#bb0000'});
    elem.polygon.destroy();
    polygon.on("click", onTileClick);
    elem.polygon = polygon;
}

function OnZoom(){
    var viewport = map._impl.modules.camera.getViewportVertices(),
        origx = Math.floor(viewport[0][0] / 131072) * 131072,
        y = Math.floor(viewport[0][1] / 131072) * 131072,
        i = 0;
    while (i < Tiles.length){
        if (!Tiles[i].Selected){
            Tiles[i].polygon.destroy();
            Tiles[i] = Tiles[Tiles.length - 1];
            Tiles.length--;
            i--;
        }
        i++;
    };
    if (map.getZoom() > 13)
        do {
            x = origx;
            do{
                if (!Tiles.find((element) => (element.x==x)&&(element.y==y))){
                   const x1 = x*Global2Lng,
                         y1 = Math.atan(Math.sinh(y/EARTH_RADIUS))*Rad2Grad,
                         x2 = (x+131072)*Global2Lng,
                         y2 = Math.atan(Math.sinh((y+131072)/EARTH_RADIUS))*Rad2Grad;
                   Tiles.push({polygon: new mapgl.Polygon(map, {coordinates: [[[x1, y1], [x2, y1], [x2, y2], [x1, y2], [x1, y1]]],
                                                                color: '#FFFFFF00',
                                                                strokeWidth: 3,
                                                                strokeColor: '#bb0000'}),x:x,y:y,Selected:false});
                   Tiles[Tiles.length - 1].polygon.on("click", onTileClick);
                }
                x += 131072;
            } while (x < viewport[2][0]);
            y += 131072;
        } while (y < viewport[2][1]);
}

function onResize(){
    const canvas = map._impl.getCanvas(),
          style = canvas.style,
          {innerWidth: width,
           innerHeight: height} = window;
    canvas.width = width;
    canvas.height = height;
    style.width = width;
    style.height = height;
    map._impl.setViewport(width, height);
}

function OnClose(){
  Socket.close();
}

map = new mapgl.Map('container',{zoom:2,zoomControl:false,key:JakartaVectorTilesKey});
map._impl.configureFloors({activeGroup: "metro",allowedFloorIds: []});

map._impl.events.zoomend = [OnZoom];
map._impl.events.moveend = [OnZoom];
map._impl.state.disablePitchByUserInteraction=true;
map._impl.state.disableRotationByUserInteraction=true;
window.onunload=OnClose;
Socket = new WebSocket("ws://127.0.0.1:"+Port);
Socket.onerror=WebSocketError;
addEventListener("resize", onResize);
OnZoom();
const control = new mapgl.Control(map, '<button onClick="disabled=true;onExport()">Экспорт в CorelDraw</button>', {position: 'topLeft'});