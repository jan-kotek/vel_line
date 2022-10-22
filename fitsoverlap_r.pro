; Program for overlaping two fits images
pro fitsoverlap_r_event, event
;This procedure is the event handler for set of buttons.
WIDGET_CONTROL, event.id, GET_UVALUE = eventvalue
device, decomposed=0
loadct,3


;jk: changes > sxpar err > solution: max. intensity (1) of graphs is maximum intensity of image, not of selected segment only
;              1 point fit err. > wrong names of variables (copy-paste mistake probably)
 ;         ?: location:  search for: datamax


;slitrotate is separate procedure, it is used before One point fit
;or before Two points fit, only if necessary to rotate slit

case eventvalue of
'slitrotate':begin   
file=dialog_pickfile()
image=readfits(file)
imagedims=size(image, /dimensions)
window, 0, xsize=imagedims[0], ysize=imagedims[1]
tvscl, image

!mouse.button = 1
i=0
x0s=intarr(2)
y0s=intarr(2)
while (i lt 2) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x0s[i]=x
    y0s[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  tvscl, image
  endif else begin
  tvscl, image
  plots, x0s[0:check],y0s[0:check], psym=1, /device
  endelse
endwhile
wdelete, 0

vec0s=float([(x0s[1]-x0s[0]),(y0s[1]-y0s[0])])
c0=vec0s[1]/(vec0s[0]+0.001)
u0=atan(c0)*(180/!dpi)
if vec0s[0] lt 0 then u0=u0+180 else begin
  if u0 lt 0 then u0=u0+360
  end
urots=u0-90
urotrads=double(abs((urots*(!dpi/180))))

a=float(max(imagedims))
b=float(min(imagedims))
r=(sqrt(a^2+b^2))/2
sizemods=round(r-b/2)
end

;open HMI image and determine solar radius in pixels to get acrs/pixel ratio needed
;for scaling the background image
;we will call background image (or picture) the image that will not undergo any changes
;that corrupt the data stored in it, and the one that will be scaled in acrseconds
;the foreground picture (also unscaled) will be the picture to undergo the
;processing and fit the background picture with regards to size, position and orientation
    'radius' : begin ;open file to read previously saved radiuses
        common share2, field2
        common sharevar7, r1
        common share1, field1
        common sharevar1, r0
        cesta = dialog_pickfile()
        openr, 3, cesta
        readf, 3 , r0, r1
        close, 3
         ;print values for check
        print, r0  
        print, r1
      end
    'HMI' : begin
    common share1, field1
    common sharevar1, r0
      file=dialog_pickfile(/read) ;reads the file path specified by the user in the native enviroment of the operating system
        widget_control,field1,set_value=file ;sets the value for field widget, for the user to have reference which file has he chosen
        widget_control,field1,/realize
      r0=radiusfits(file) ;calls function to calculate solar radius in pixels
      
     end
      
     
;open the unscaled image with limb, determine radius and calculate resolution
;the code does the same as above, only for the unscaled picture
  'unscale' : begin
  common share2, field2
  common sharevar7, r1
  
    file=dialog_pickfile(/read)
      widget_control,field2,set_value=file
      widget_control,field2,/realize
    r1=radiusfits(file)

 ;write variables r0 and r1 to file
   otazka = dialog_message("Would you like to save measured radius into file radius.dat (existing data will be overwritten)?", /question, title="Save to radius.dat")
   case otazka of 
   'Yes': begin
      slozka=dialog_pickfile(/directory, title='Select the directory to save radius.dat')   
      fname=slozka + 'radius.dat'
      openw, 2, fname
      printf,2, r0, r1
      close, 2
      end
  else:print, "OK"
      endcase
;;open first image without limb and determine size
end
  'firstimage':begin
  common share3, field3
  common sharevar2, image1, imagedims1, x0,y0
  
  file=dialog_pickfile(/read)
  image1=readfits(file, header1) ;reads image and header to two separete arrays of appropriate type (two dimensional number and string arrays respectively)
  imagedims1=size(image1, /dimensions) ;reads the image dimensions - easier and faster than to manipulate and read from header
      widget_control,field3,set_value=file
      widget_control,field3,/realize
      
;pick two disctinctive points on scaled image
;the draw widget is used to display the pictures, because window function does not
;support scrollbars, which are important for pictures that are bigger than the screen

based=widget_base(title=file , xsize=700, ysize=700) ;create a base to fit the screen
draw=widget_draw(based, xsize=imagedims1[0], ysize=imagedims1[1], $
    x_scroll_size=640, y_scroll_size=650) ;create a draw widget for displaying images
widget_control, based, /realize
widget_control, draw, get_value=windex ;get value of window index (draw widget index)
wset, windex ;set the created widget to be active
tvscl, image1 ;display the image on the active widget

i=0 ;set counter to zero
x0=intarr(2) ;create an array for x coordinates to be stored
y0=intarr(2) ;create an array for y coordinates to be stored
while (i lt 2) do begin ;waits until two points are placed on the picture
  cursor, x,y, /device, /down ;loads current pointer device coordinates into x and y variables, with "button is down" (any button is pressed) triger
  case !mouse.button of ;reads the system variable !mouse.button that stores the last button action on the mouse: 1 being left and 4 right mouse button
  1: begin
    x0[i]=x
    y0[i]=y
    i=i+1
    end
  4: i=i-1
  endcase ;in case left mouse button is pressed it stores the coordinates into variables, and right mouse button takes the user one step back by lowering the counter by one
check=i-1 ;a variable used for checking the counter status, used for easy error evasion
if check lt 0 then begin
  i=0
  check=0
  tvscl, image1 ;if the check variable is (-1) then reset the process
             ;this is used when the counter is zero (no points placed on the image) and the user presses the right mouse button - as mentioned above this is error evasion
  endif else begin
  tvscl, image1
  plots, x0[0:check],y0[0:check], psym=1, color='ff'xl, /device
  endelse ;if the user presses the right mouse button, the point is marked with a red cross on the picture
endwhile
widget_control, based, /destroy ;closes the widget so it doesn't take up memory
end  

;open seconond image without limb and determine size
;the same process described above is repeated for the second picture (the one to be
;processed) only the variable names used to store the measured values are different

'secondimage':begin
common share4, field4
common sharevar2, image1, imagedims1, x0,y0
common sharevar3, x1,y1, image2, imagedims2
common sharevar4, urot


file=dialog_pickfile(/read)
image2=readfits(file, header2)
image2= rebin(image2, 640, 512) ; rebinning of sj
imagedims2=size(image2, /dimensions)  
      widget_control,field4,set_value=file
      widget_control,field4,/realize

;pick two distinctive points on unscaled image

based=widget_base(title=file , xsize=700, ysize=700)
draw=widget_draw(based, xsize=imagedims2[0], ysize=imagedims2[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, based, /realize
widget_control, draw, get_value=windex
wset, windex
tvscl, image2

!mouse.button = 1
i=0
x1=intarr(2)
y1=intarr(2)
while (i lt 2) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x1[i]=x
    y1[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  tvscl, image2
  endif else begin
  tvscl, image2
  plots, x1[0:check],y1[0:check], psym=1, color='ff'xl, /device
  endelse
endwhile
widget_control, based, /destroy
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;repeat (jfk)


;open first image without limb and determine size
'repeatit':begin
  common share3, field3
  common sharevar2, image1, imagedims1, x0,y0
  
 
  imagedims1=size(image1, /dimensions) ;reads the image dimensions - easier and faster than to manipulate and read from header
      widget_control,field3,set_value=file
      widget_control,field3,/realize
      
;pick two disctinctive points on scaled image
;the draw widget is used to display the pictures, because window function does not
;support scrollbars, which are important for pictures that are bigger than the screen

based=widget_base(title=file , xsize=700, ysize=700) ;create a base to fit the screen
draw=widget_draw(based, xsize=imagedims1[0], ysize=imagedims1[1], $
    x_scroll_size=640, y_scroll_size=650) ;create a draw widget for displaying images
widget_control, based, /realize
widget_control, draw, get_value=windex ;get value of window index (draw widget index)
wset, windex ;set the created widget to be active
tvscl, image1 ;display the image on the active widget

i=0 ;set counter to zero
x0=intarr(2) ;create an array for x coordinates to be stored
y0=intarr(2) ;create an array for y coordinates to be stored
while (i lt 2) do begin ;waits until two points are placed on the picture
  cursor, x,y, /device, /down ;loads current pointer device coordinates into x and y variables, with "button is down" (any button is pressed) triger
  case !mouse.button of ;reads the system variable !mouse.button that stores the last button action on the mouse: 1 being left and 4 right mouse button
  1: begin
    x0[i]=x
    y0[i]=y
    i=i+1
    end
  4: i=i-1
  endcase ;in case left mouse button is pressed it stores the coordinates into variables, and right mouse button takes the user one step back by lowering the counter by one
check=i-1 ;a variable used for checking the counter status, used for easy error evasion
if check lt 0 then begin
  i=0
  check=0
  tvscl, image1 ;if the check variable is (-1) then reset the process
             ;this is used when the counter is zero (no points placed on the image) and the user presses the right mouse button - as mentioned above this is error evasion
  endif else begin
  tvscl, image1
  plots, x0[0:check],y0[0:check], psym=1, color='ff'xl, /device
  endelse ;if the user presses the right mouse button, the point is marked with a red cross on the picture
endwhile
widget_control, based, /destroy ;closes the widget so it doesn't take up memory


;open seconond image without limb and determine size
;the same process described above is repeated for the second picture (the one to be
;processed) only the variable names used to store the measured values are different



imagedims2=size(image2, /dimensions)
      widget_control,field4,set_value=file
      widget_control,field4,/realize

;pick two distinctive points on unscaled image

based=widget_base(title=file , xsize=700, ysize=700)
draw=widget_draw(based, xsize=imagedims2[0], ysize=imagedims2[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, based, /realize
widget_control, draw, get_value=windex
wset, windex
tvscl, image2

!mouse.button = 1
i=0
x1=intarr(2)
y1=intarr(2)
while (i lt 2) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x1[i]=x
    y1[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  tvscl, image2
  endif else begin
  tvscl, image2
  plots, x1[0:check],y1[0:check], psym=1, color='ff'xl, /device
  endelse
endwhile
widget_control, based, /destroy
end
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<repeat (jfk)


;data calculation for translation, rotation before resize
'coeff': begin
    common sharevar1, r0
    common sharevar2, image1, imagedims1, x0,y0
    common sharevar3, x1,y1, image2, imagedims2
    common sharevar4, urot
    common sharevar5, reskoef, vectran
    common sharevar7, r1
    common sharevar8, sizemod
    common sharevar9, res1

;resolution
res0=2.2 ;known resolution of the camera. for HMI it is 0.5 arcseconds per pixel
res1=(r0[0]/r1[0])*res0 ;calculates the resolution of the unscaled background image

;rotation koefs - calculates the angle for rotation. direction depends on the sign of the angle
vec0=float([(x0[1]-x0[0]),(y0[1]-y0[0])]) ;vector determined by the user placed coordinates on the background image
vec1=float([(x1[1]-x1[0]),(y1[1]-y1[0])]) ;vector determined by the user placed coordinates on the foreground image
c0=vec0[1]/(vec0[0]+0.001) ;koeficient representing tan(alpha), alpha being the angle of the vector with respect to x axis (background image)
c1=vec1[1]/(vec1[0]+0.001) ;and the same for the foreground image
u0=atan(c0)*(180/!dpi) ;the above mentioned angle (for foreground and then
u1=atan(c1)*(180/!dpi) ;background image)

if vec0[0] lt 0 then u0=u0+180 else begin
  if u0 lt 0 then u0=u0+360
  end
if vec1[0] lt 0 then u1=u1+180 else begin
  if u1 lt 0 then u1=u1+360
  end ;determines the absolute angle values (0-360 degrees) again with the respect to x axis, measured counter clockwise
urot=u1-u0 ;angle of rotation as a difference betweet the two vectors (can be negative - the sign changes the rotation direction, with + being clockwise and vice versa)
urotrad=double(abs((urot*(!dpi/180)))) ;conversion to radians (needed later)

;resize koefs
int0=sqrt(vec0[0]^2+vec0[1]^2) ;intensities of the two vectors
int1=sqrt(vec1[0]^2+vec1[1]^2)
reskoef=int0/int1 ;the resize koeficient, calculated from the intensities of two vectors

;size modification koefs for rotation without data loss (explained later)
a=float(max(imagedims2)) ;larger side of the image rectangle
b=float(min(imagedims2)) ;smaller side of the image rectangle
; this is provided the image has two different dimensions, but will work just as well if the image is a square (both sides are same length)
r=(sqrt(a^2+b^2))/2 ;half diagonal of the rectangle
sizemod=round((r-b/2)*abs(sin(urotrad))) ;the amount of black space to be added to each side of the picture (like a black frame)
;this is necessary for rotation without data loss - otherwise some parts of the image (mostly edges) would be lost

;translation koefs
;for the correct translation it is needed to calculate the new coordinates of
;the first point (which is used for translation) placed by the user. the change is due
;to rotation and resize being done before translation. basic geometry is used. it is
;needed to keep in mind that the rotation function rotates the image around it's
;center
xn1=double(x1[0]+sizemod)
yn1=double(y1[0]+sizemod)
xn5=((x1[0]-imagedims2[0]/2)*cos(urotrad)+(y1[0]-imagedims2[1]/2)*sin(urotrad)*(urot/abs(urot))+imagedims2[0]/2+sizemod)*reskoef
yn5=((y1[0]-imagedims2[1]/2)*cos(urotrad)-(x1[0]-imagedims2[0]/2)*sin(urotrad)*(urot/abs(urot))+imagedims2[1]/2+sizemod)*reskoef
vectran=round([x0[0]-xn5,y0[0]-yn5])

;************* coefs export *********
 otazka = dialog_message("Would you like to save coefficients into file ?", /question, title="Save to")
   case otazka of 
   'Yes': begin
slozka=dialog_pickfile(/directory)
for i=1, 10 do begin
  fname = slozka + 'coeffs_' + string(i) + '.dat'
  vysledek_testu = file_test(fname)
  if vysledek_testu eq 0 then begin
    openw, 20, fname
    printf,20, urot, reskoef, sizemod, vectran
    close, 20
    break
    endif
  if i eq 20 then begin
    zprava = 'You have reached maximum of stored files. Please remove or rename some of them.'
    x = dialog_message(zprava, /error)
  endif
endfor
end
  else:print, "OK"
      endcase
;***********************************
end


;modification of image size for rotation without data loss
;adds a black frame (zero bytes) around the picture rotation of the fits image
;see rotmodfits function for details
'rotation': begin
  common sharevar2, image1, imagedims1, x0,y0
  common sharevar3, x1,y1, image2, imagedims2
  common sharevar4, urot
  common sharevar5, reskoef, vectran
  common sharevar8, sizemod

image2=rotmodfits(image2, imagedims2, sizemod, urot) ;variables: image, dimensions, size modification scalar, and angle of rotation
imagedims2=size(image2, /dimensions) ;get new dimensions
end

;resize. the congrid function is used.
'resize':begin
  common sharevar3, x1,y1, image2, imagedims2
  common sharevar5, reskoef, vectran
  common sharevar9, res1

image2=resizefits(image2, imagedims2, reskoef) ;variables: image, dimensions, resize koeficient
imagedims2=size(image2, /dimensions) ;get new dimensions

;show scaled pictures
;based=widget_base(title=file , xsize=700, ysize=700)
;draw1=widget_draw(based, xsize=imagedims1[0], ysize=imagedims1[1], $
;    x_scroll_size=640, y_scroll_size=650)
;widget_control, based, /realize
;widget_control, draw1, get_value=windex1
;wset, windex1
;tvscl, image1
;ticksize=round(10/res1) ;note that ticksize is actually a variable that is used for tickinterval parameter. in this case the tick intervals are aprox. 10 arcseconds
;plot, [0,imagedims1[0]],[0,imagedims1[1]], /nodata, /device, /noerase, $
;    position=[0,0,imagedims1[0],imagedims1[1]], $
;    xtickinterval=ticksize, ytickinterval=ticksize ;this nodata plot is used to draw the ticks on the picture

;based2=widget_base(title=file , xsize=700, ysize=700)
;draw2=widget_draw(based2, xsize=imagedims2[0], ysize=imagedims2[1], $
;    x_scroll_size=640, y_scroll_size=650)
;widget_control, based2, /realize
;widget_control, draw2, get_value=windex2
;wset, windex2
;tvscl, image2
;ticksize=round(10/res1)
;plot, [0,imagedims2[0]],[0,imagedims2[1]], /nodata, /device, /noerase, $
;   position=[0,0,imagedims2[0],imagedims2[1]], $
;    xtickinterval=ticksize, ytickinterval=ticksize
end

;modification of image size for correct translation
;adds black space to compensate for the translation
;see translatefits function for details
'translation':begin
  common sharevar3, x1,y1, image2, imagedims2
  common sharevar5, reskoef, vectran
  common sharevar6, status
  
image2=translatefits(image2, imagedims2, vectran)
imagedims2=size(image2, /dimensions)

;modification for transparency by adding black space (as with rotation and translation)
;transparency only works if the images are of the exact same size

maxx=max([imagedims1[0],imagedims2[0]])
maxy=max([imagedims1[1],imagedims2[1]])

dummy=image1
image1=lonarr(maxx, maxy)
  for j=0, (imagedims1[0]-1) do begin
    for k=0, (imagedims1[1]-1) do begin
      image1[j,k]=dummy[j,k]
    endfor
  endfor

imagedims1=size(image1, /dimensions)

dummy=image2
image2=lonarr(maxx, maxy)
  for j=0, (imagedims2[0]-1) do begin
    for k=0, (imagedims2[1]-1) do begin
      image2[j,k]=dummy[j,k]
    endfor
  endfor
druhy_obrazek=ptr_new(image2) 
imagedims2=size(image2, /dimensions)
end

'a': begin
    status=event.value
    end
    
    
;transparency
'transparency':begin
  common sharevar2, image1, imagedims1, x0,y0
  common sharevar3, x1,y1, image2, imagedims2
  common sharevar6, status
  common get, mix
  common sdilene, druhy
  druhy=image2
a=status ;the value is taken from the slider in the interface
;<jk check>
;difference=abs(image1-image2)
;writefits,'difference.fits',difference
;tvscl,difference
;print,'difference done'
;</jk check>
mix=long(a*float(image1)+(1-a)*float(image2)) ;creates a new image.
;the result of this operation is the sum of two matching pixels from each image
;timed by (a) and (1-a) for background and foreground image respectively.
;this appears to the human eye as transparency
mixdims=float(size(mix, /dimensions))
based=widget_base(title='final fit' , xsize=700, ysize=700)
draw=widget_draw(based, xsize=mixdims[0], ysize=mixdims[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, based, /realize
widget_control, draw, get_value=windex
wset, windex

tvscl, mix
;********* export of the mixed picture **********
 otazka = dialog_message("Would you like to save the image? ?", /question, title="Save it?")
   case otazka of 
   'Yes': begin
      
      
slozka=dialog_pickfile(/directory)
for i=1, 20 do begin
  fname = slozka + 'transparency_' + string(i) + '.fits'
  vysledek_testu = file_test(fname)
  if vysledek_testu eq 0 then begin
    writefits, fname, mix
    break
    endif
  if i eq 20 then begin
  zprava = 'You have reached maximum of stored files. Please remove or rename some of them.'
  x = dialog_message(zprava, /error)
  endif
endfor
  end
  else:print, "OK"
      endcase
;************************************************
ticksize=round(10/res1)
plot, [0,mixdims[0]],[0,mixdims[1]], /nodata, /device, /noerase, $
    position=[0,0,mixdims[0],mixdims[1]], $
    xtickinterval=ticksize, ytickinterval=ticksize
end

'final':begin
  common sharevar2, image1, imagedims1, x0,y0
  common sharevar3, x1,y1, image2, imagedims2
  common sharevar6, status
  common get, mix
a=status ;the value is taken from the slider in the interface
;<jk check>
;difference=abs(image1-image2)
;writefits,'difference.fits',difference
;tvscl,difference
;print,'difference done'
;</jk check>
;mix=long(a*float(image1)+(1-a)*float(image2)) ;creates a new image.
;jk>
mix=[[[IMAGE1]], [[IMAGE2]],[[IMAGE1]]]
;<jk
;the result of this operation is the sum of two matching pixels from each image
;timed by (a) and (1-a) for background and foreground image respectively.
;this appears to the human eye as transparency
mixdims=float(size(mix, /dimensions))
based=widget_base(title='final fit' , xsize=700, ysize=700)
draw=widget_draw(based, xsize=mixdims[0], ysize=mixdims[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, based, /realize
widget_control, draw, get_value=windex
wset, windex

tvscl, mix, true=3
ticksize=round(10/res1)
plot, [0,mixdims[0]],[0,mixdims[1]], /nodata, /device, /noerase, $
    position=[0,0,mixdims[0],mixdims[1]], $
    xtickinterval=ticksize, ytickinterval=ticksize
end

'getintensity':begin
        common get, mix
      
      ;get the specter from both pictures on the slit position
based=widget_base(title=file , xsize=700, ysize=700) ;create a base to fit the screen
draw=widget_draw(based, xsize=imagedims1[0], ysize=imagedims1[1], $
    x_scroll_size=640, y_scroll_size=650) ;create a draw widget for displaying images
widget_control, based, /realize
widget_control, draw, get_value=windex ;get value of window index (draw widget index)
wset, windex ;set the created widget to be active
TVSCL, mix
i=0
x3=intarr(2)
y3=intarr(2)
while (i lt 2) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x3[i]=x
    y3[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  tvscl, mix
  endif else begin
  tvscl, mix
  plots, x3[0:check],y3[0:check], psym=1, /device, color='ff'xl
  endelse
endwhile
;widget_control, base, /destroy

vslit=float([x3[1]-x3[0],y3[1]-y3[0]])
intvslit=round(sqrt(vslit[0]^2+vslit[1]^2))
kslit=vslit[1]/(vslit[0]+0.00001)
xpos=fltarr(intvslit)+x3[0]
ypos=fltarr(intvslit)+y3[0]

for i=0,intvslit-2 do begin
  xpos[i+1]=xpos[i]+(vslit[0]/intvslit)
  ypos[i+1]=ypos[i]+(vslit[1]/intvslit)
  endfor
xpos=round(xpos)-1
ypos=round(ypos)-1


datamax1=max(image1)
datamax2=max(image2)
print, datamax1, datamax2
int1=float(image1[xpos, ypos])/datamax1
int2=float(image2[xpos, ypos])/datamax2

window, 0
plot, int1, ypos, psym=1, xrange=[min(int1),max(int1)], $
    yrange=[min(ypos),max(ypos)]
window, 1
plot, int2, ypos, psym=1, xrange=[min(int2),max(int2)], $
    yrange=[min(ypos),max(ypos)], color='ff'xl
window, 2
plot, int2, int1, psym=1, xrange=[min(int2),max(int2)], yrange=[min(int1),max(int1)]

;************* graph export *********
 otazka = dialog_message("Would you like to save measured intenities into file ?", /question, title="Save to")
   case otazka of 
   'Yes': begin
slozka=dialog_pickfile(/directory)
for i=1, 10 do begin
  fname = slozka + 'intensity_graph_' + string(i) + '.dat'
  vysledek_testu = file_test(fname)
  if vysledek_testu eq 0 then begin
    openw, 20, fname
    printf,20, float(image2[xpos, ypos])
    close, 20
    break
    endif
  if i eq 20 then begin
    zprava = 'You have reached maximum of stored files. Please remove or rename some of them.'
    x = dialog_message(zprava, /error)
  endif
endfor
end
  else:print, "OK"
      endcase
;***********************************
goto, abort
end
;;;;;;;;;;;;;;;;;;;;;
"correlate": begin
  common sharevar2, image1, imagedims1, x0,y0
  common sharevar3, x1,y1, image2, imagedims2
  common sharevar6, status
  common get, mix
 
;openw, 10, "D:\ondrejov\image1.dat"
;printf, 10, image1
;close,10
  a=ULONG(0)
  hodnota1 = make_array(4000000, /nozero) ;array for storing data from each pixel
  hodnota2 = make_array(4000000, /nozero)
for i=0, imagedims1[1]-1, 1 do begin      ;this finds every nonzero pixel from second image and store itsvalue and the according value from first image
  for j=0, imagedims1[0]-1, 1 do begin
    ;print,i,j
    if image2[i,j] ne 0 then begin
    hodnota1[a] = image1[i,j]
    hodnota2[a] = image2[i,j]
    a=a+1
    endif
  endfor
endfor
konecne1 = make_array(a, /nozero)  ;clean array
konecne2 = make_array(a, /nozero)
for p=ulong(0), a-1, 1 do begin
konecne1[p]=hodnota1[p]
konecne2[p]=hodnota2[p]
endfor
plot, konecne1, konecne2, psym=1
vysledek = correlate(konecne1, konecne2, /double)
rank = r_correlate(konecne1, konecne2)
print, "Correlation coefficient: ", vysledek
print, "Rank and significance: ", rank
end

'coef': begin
    common sharevar4, urot
    common sharevar5, reskoef, vectran
    common sharevar8, sizemod
        cesta = dialog_pickfile()
        openr, 4, cesta
        readf, 4, urot, reskoef, sizemod, vectran
        close, 4
         ;print values for check
        print, "Rotation: ", urot
        print, "Resize: ", reskoef
        print, "Sizemod: ", sizemod
        print, "Translation: ", vectran
        
end






























;one point fit is the same as two point fit only this time the program calculates the
;second point, by calculating the nearest point on the rim (or edge) of the solar disc
;from the specified disctinctive point. the radius in pixels, position of the center
;of the disc and user placed point coordinates are used in the calculation

;open HMI image and determine solar radius in pixels
 'HMI1' : begin
    common share5, field5
    common share1var1, r01
      file=dialog_pickfile(/read)
        print, file
        x=file
        widget_control,field5,set_value=x
        widget_control,field5,/realize
        file=x
      r01=radiusfits(file) 
     end
     
;open background image and determine radius of limb
     
     'unscale1' : begin
  common share6, field6
  common share1var7, r11
  
    file=dialog_pickfile(/read)
      widget_control,field6,set_value=file
      widget_control,field6,/realize
    r11=radiusfits(file)
  end
  
;load image and determine size
'firstimage1':begin
  common share7, field7
  common share1var2, image11, imagedims11, x01,y01
  common share1var7, r11
  
  
  file=dialog_pickfile(/read)
  image11=readfits(file, header11)
  imagedims11=size(image11, /dimensions)
      widget_control,field7,set_value=file
      widget_control,field7,/realize

;pick one reference point on background image

base=widget_base(title=file , xsize=700, ysize=700)
draw=widget_draw(base, xsize=imagedims11[0], ysize=imagedims11[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, base, /realize
widget_control, draw, get_value=windex
wset, windex
TVSCL, image11

i=0
x01=intarr(3)
y01=intarr(3)
while (i lt 1) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x01[i]=x
    y01[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  TVSCL, image11
  endif else begin
  TVSCL, image11
  plots, x01[0:check],y01[0:check], psym=1, /device, color='ff'xl
  endelse
endwhile

;calculation for the second point - the circle and line equasions are used
;as well as vector magnitudes for comparation which of the two calculated points
;is closer (as the equasions give two correct values on both oposite sides of 
;the center)

v0=double([x01[0]-r11[2], y01[0]-r11[3]])
k0=v0[1]/v0[0]
a=1+k0^2
b=(-2)*r11[2]*a
c=a*(r11[2]^2)-r11[0]^2
x01[1]=round(((-1)*b+sqrt(b^2-4*a*c))/(2*a))
y01[1]=round(k0*(x01[1]-r11[2])+r11[3])
x01[2]=round(((-1)*b-sqrt(b^2-4*a*c))/(2*a))
y01[2]=round(k0*(x01[2]-r11[2])+r11[3])
v1=long([x01[1]-x01[0], y01[1]-y01[0]])
v2=long([x01[2]-x01[0], y01[2]-y01[0]])
if (v2[0]^2+v2[1]^2) lt (v1[0]^2+v1[1]^2) then begin
  x01[1]=x01[2]
  y01[1]=y01[2]
  endif
      end
      
      
      ;open foreground image and determine radius of limb

'secondimage1':begin
common share8, field8
common share1var2, image11, imagedims11, x01,y01
common share1var3, x11,y11, image21, imagedims21
common share1var4, urot1

;load file path and determine radius on foreground image

file=dialog_pickfile(/read)
r21=radiusfits(file)

;load image and determine size

image21=readfits(file, header22)
imagedims21=size(image21, /dimensions)
      widget_control,field8,set_value=file
      widget_control,field8,/realize
      
;pick one reference point on foreground image

base=widget_base(title=file , xsize=700, ysize=700)
draw=widget_draw(base, xsize=imagedims21[0], ysize=imagedims21[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, base, /realize
widget_control, draw, get_value=windex
wset, windex
TVSCL, image21

!mouse.button = 1
i=0
x11=intarr(3)
y11=intarr(3)
while (i lt 1) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x11[i]=x
    y11[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  TVSCL, image21
  endif else begin
  TVSCL, image21
  plots, x11[0:check],y11[0:check], psym=1, /device, color='ff'xl
  endelse
endwhile
widget_control, base, /destroy

v0=double([x11[0]-r21[2], y11[0]-r21[3]])
k0=v0[1]/(v0[0]+0.0001)
a=1+k0^2
b=(-2)*r21[2]*a
c=a*(r21[2]^2)-r21[0]^2
x11[1]=round(((-1)*b+sqrt(b^2-4*a*c))/(2*a))
y11[1]=round(k0*(x11[1]-r21[2])+r21[3])
x11[2]=round(((-1)*b-sqrt(b^2-4*a*c))/(2*a))
y11[2]=round(k0*(x11[2]-r21[2])+r21[3])
v1=long([x11[1]-x11[0], y11[1]-y11[0]])
v2=long([x11[2]-x11[0], y11[2]-y11[0]])
if (v2[0]^2+v2[1]^2) lt (v1[0]^2+v1[1]^2) then begin
  x11[1]=x11[2]
  y11[1]=y11[2]
  endif
      end

;the rest of the code is the same as two point fit - the code uses the two calculated
;points as secondary points
      
      ;data calculation for translation, rotation before resize
'coeff1': begin
    common share1var1, r01
    common share1var2, image11, imagedims11, x01,y01
    common share1var3, x11,y11, image21, imagedims21
    common share1var4, urot1
    common share1var5, reskoef1, vectran1
    common share1var7, r11
    common share1var8, sizemod1
    common share1var9, res11
   
;resolution
res0=2.2
res11=(r01[0]/r11[0])*res0

;rotation koefs
vec0=float([(x01[1]-x01[0]),(y01[1]-y01[0])])
vec1=float([(x11[1]-x11[0]),(y11[1]-y11[0])])
c0=vec0[1]/(vec0[0]+0.0001)
c1=vec1[1]/(vec1[0]+0.0001)
u0=atan(c0)*(180/!dpi)
u1=atan(c1)*(180/!dpi)

if vec0[0] lt 0 then u0=u0+180 else begin
  if u0 lt 0 then u0=u0+360
  end
if vec1[0] lt 0 then u1=u1+180 else begin
  if u1 lt 0 then u1=u1+360
  end
urot1=u1-u0
urotrad=double(abs((urot1*(!dpi/180))))

;resize koefs
int0=sqrt(vec0[0]^2+vec0[1]^2)
int1=sqrt(vec1[0]^2+vec1[1]^2)
reskoef1=int0/int1

;size modification koefs for rotation without data loss
a=float(max(imagedims21))
b=float(min(imagedims21))
r=(sqrt(a^2+b^2))/2
sizemod1=round((r-b/2))

;translation koefs
xn1=double(x11[0]+sizemod1)
yn1=double(y11[0]+sizemod1)
xn5=((x11[0]-imagedims21[0]/2)*cos(urotrad)+(y11[0]-imagedims21[1]/2)*sin(urotrad)*(urot1/abs(urot1))+imagedims21[0]/2+sizemod1)*reskoef1
yn5=((y11[0]-imagedims21[1]/2)*cos(urotrad)-(x11[0]-imagedims21[0]/2)*sin(urotrad)*(urot1/abs(urot1))+imagedims21[1]/2+sizemod1)*reskoef1
vectran1=round([x01[0]-xn5,y01[0]-yn5])
end

;modification of image size for rotation without data loss
;adds a black frame (zero bytes) around the picture + rotation of a bitpix image
;provided in data block of the fits file
'rotation1': begin
  common share1var2, image11, imagedims11, x01,y01
  common share1var3, x11,y11, image21, imagedims21
  common share1var4, urot1
  common share1var5, reskoef1, vectran1
  common share1var8, sizemod1

image21=rotmodfits(image21, imagedims21, sizemod1, urot1)
imagedims21=size(image21, /dimensions)
end

;resize
'resize1':begin
  common share1var3, x11,y11, image21, imagedims21
  common share1var5, reskoef1, vectran1
  common share1var9, res11
image21=resizefits(image21, imagedims21, reskoef1)
imagedims21=size(image21, /dimensions)

;show scaled pictures
base1=widget_base(title=file , xsize=700, ysize=700)
draw1=widget_draw(base1, xsize=imagedims11[0], ysize=imagedims11[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, base1, /realize
widget_control, draw1, get_value=windex1
wset, windex1
TVSCL, image11
ticksize=round(10/res11)
plot, [0,imagedims11[0]],[0,imagedims11[1]], /nodata, /device, /noerase, $
    position=[0,0,imagedims11[0],imagedims11[1]], $
    xtickinterval=ticksize, ytickinterval=ticksize

base2=widget_base(title=file , xsize=700, ysize=700)
draw2=widget_draw(base2, xsize=imagedims21[0], ysize=imagedims21[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, base2, /realize
widget_control, draw2, get_value=windex2
wset, windex2
TVSCL, image21
ticksize=round(10/res11)
plot, [0,imagedims21[0]],[0,imagedims21[1]], /nodata, /device, /noerase, $
    position=[0,0,imagedims21[0],imagedims21[1]], $
    xtickinterval=ticksize, ytickinterval=ticksize
end
      
      ;modification of image size for correct translation
;adds black space to compensate for the translation
'translation1':begin
  common share1var3, x11,y11, image21, imagedims21
  common share1var5, reskoef1, vectran1
  common share1var6, status1
image21=translatefits(image21, imagedims21, vectran1)
imagedims21=size(image21, /dimensions)

;resize for transparency by adding black space

maxx=max([imagedims11[0],imagedims21[0]])
maxy=max([imagedims11[1],imagedims21[1]])

dummy=image11
image11=lonarr(maxx, maxy)
  for j=0, (imagedims11[0]-1) do begin
    for k=0, (imagedims11[1]-1) do begin
      image11[j,k]=dummy[j,k]
    endfor
  endfor

imagedims11=size(image11, /dimensions)

dummy=image21
image21=lonarr(maxx, maxy)
  for j=0, (imagedims21[0]-1) do begin
    for k=0, (imagedims21[1]-1) do begin
      image21[j,k]=dummy[j,k]
    endfor
  endfor
  
imagedims21=size(image21, /dimensions)
end




'a1': begin
    status1=event.value
    end
    
;transparency
'transparency1':begin
  common share1var2, image11, imagedims11, x01,y01
  common share1var3, x11,y11, image21, imagedims21
  common share1var6, status1
  common get1, mix1
  common share1var9, res11

;transparency

a=status1
mix1=a*float(image11)+(1-a)*float(image21)
mixdims=float(size(mix1, /dimensions))
base=widget_base(title='final fit' , xsize=700, ysize=700)
draw=widget_draw(base, xsize=mixdims[0], ysize=mixdims[1], $
    x_scroll_size=640, y_scroll_size=650)
widget_control, base, /realize
widget_control, draw, get_value=windex
wset, windex
TVSCL, mix1
ticksize=round(10/res11)
plot, [0,mixdims[0]],[0,mixdims[1]], /nodata, /device, /noerase, $
    position=[0,0,mixdims[0],mixdims[1]], $
    xtickinterval=ticksize, ytickinterval=ticksize
      end
      
 'getintensity1':begin
      common get1, mix1
      ;get the specter from both pictures on the slit position

!mouse.button = 1
i=0
x3=intarr(2)
y3=intarr(2)
while (i lt 2) do begin
  cursor, x,y, /device, /down
  case !mouse.button of
  1: begin
    x3[i]=x
    y3[i]=y
    i=i+1
    end
  4: i=i-1
  endcase
check=i-1
if check lt 0 then begin
  i=0
  check=0
  TVSCL, mix1
  endif else begin
  TVSCL, mix1
  plots, x3[0:check],y3[0:check], /device, color='ff'xl
  endelse
endwhile

vslit=float([x3[1]-x3[0],y3[1]-y3[0]])
intvslit=round(sqrt(vslit[0]^2+vslit[1]^2))
kslit=vslit[1]/(vslit[0]+0.00001)
xpos=fltarr(intvslit)+x3[0]
ypos=fltarr(intvslit)+y3[0]

for i=0,intvslit-2 do begin
  xpos[i+1]=xpos[i]+(vslit[0]/intvslit)
  ypos[i+1]=ypos[i]+(vslit[1]/intvslit)
  endfor
xpos=round(xpos)-1
ypos=round(ypos)-1

datamax1=max(image11)
datamax2=max(image21)
int1=float(image11[xpos, ypos])/datamax1
int2=float(image21[xpos, ypos])/datamax2

window, 0
plot, int1, ypos, psym=1, xrange=[min(int1),max(int1)], $
    yrange=[min(ypos),max(ypos)]
window, 1
plot, int2, ypos, psym=1, xrange=[min(int2),max(int2)], $
    yrange=[min(ypos),max(ypos)], color='ff'xl
window, 2
plot, int2, int1, psym=1, xrange=[min(int2),max(int2)], yrange=[min(int1),max(int1)]

goto, abort
end

endcase


abort:
print, 'end of program'
end

PRO fitsoverlap_r, GROUP = GROUP
    common share1, field1
    common share2, field2
    common share3, field3
    common share4, field4
    common share5, field5
    common share6, field6
    common share7, field7
    common share8, field8
    common button, base, buttonbase1
; Create the top-level base widget:   

base = WIDGET_BASE(TITLE = 'Main Menu', XSIZE = 850, ysize = 700, MBAR=bar)

buttonbase2 = widget_base(base, yoffset=10, xoffset=450, column=1) ;BASE for Two point fit interface

buttonbase1 = widget_base(base, yoffset=10, xoffset=10, column=1) ;BASE for One point fit interface
buttonbase3 = widget_base(base, yoffset=660, xoffset=215, column=4) ; Base for Slitrotate

label2=widget_label(buttonbase2, value= 'Two points fit', /align_left)
label3=widget_label(buttonbase2, value='', ysize=10)
label4=widget_label(buttonbase2, value='Resolution')
label5=widget_label(buttonbase2, value='', ysize=5)

buttsize=100

HMIbutton = WIDGET_BUTTON(buttonbase2, $                              ;The button belongs to 'base'
          VALUE = 'Open HMI image',$                                ;The button label.
          UVALUE = 'HMI', xsize=buttsize)                           ;The button's User Value.

field1 = CW_FIELD(buttonbase2,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
Scalebutton = WIDGET_BUTTON(buttonbase2,VALUE = 'Open unscaled image with limb',UVALUE = 'unscale', xsize=buttsize)
field2 = CW_FIELD(buttonbase2,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
label3=widget_label(buttonbase2, value='', ysize=10)
label4=widget_label(buttonbase2, value='Select two points')
label5=widget_label(buttonbase2, value='', ysize=5)
firsbutton = WIDGET_BUTTON(buttonbase2,VALUE = 'Open first image',UVALUE = 'firstimage', xsize=buttsize)
field3 = CW_FIELD(buttonbase2,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
secondbutton = WIDGET_BUTTON(buttonbase2,VALUE = 'Open second image',UVALUE = 'secondimage', xsize=buttsize)
field4 = CW_FIELD(buttonbase2,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
;jk>
;labeL33=widget_label(buttonbase2, value='', ysize=15)
;coeffbutton = widget_button(buttonbase2, value= 'Compute it all', uvalue='compute')
;<jk
label6=widget_label(BUTTONBASE2, value='', ysize=15)
coeffbutton = widget_button(BUTTONBASE2, value= 'Determine coefficients', uvalue='coeff')
label7=widget_label(BUTTONBASE2, value='', ysize=5)
rotbutton=widget_button(BUTTONBASE2, value='Rotation', uvalue= 'rotation')
label8=widget_label(BUTTONBASE2, value='', ysize=5)
resbutton=widget_button(BUTTONBASE2, value='Resize', uvalue = 'resize')
label9=widget_label(BUTTONBASE2, value='', ysize=5)
transbutton=widget_button(BUTTONBASE2, value='Translation', uvalue='translation')
label10=widget_label(BUTTONBASE2, value='', ysize=5)
slider = cw_fslider(BUTTONBASE2, maximum=1, minimum=0, scroll=0.1, title='Set value of transparency', $
                      value=0, uvalue='a')
label10=widget_label(BUTTONBASE2, value='', ysize=5)
transparbutton=widget_button(BUTTONBASE2, value='Transparency', uvalue='transparency')
;jfk
label30=widget_label(BUTTONBASE2, value='', ysize=5)
finalbutton=widget_button(BUTTONBASE2, value='Show RGB result', uvalue='final')
label31=widget_label(BUTTONBASE2, value='', ysize=5)
repeatbutton=widget_button(BUTTONBASE2, value='repeat two point fit', uvalue='repeatit')
;jfk

label11=widget_label(BUTTONBASE2, value='', ysize=10)

getspecterbutton=widget_button(BUTTONBASE2, value='Get intensity', uvalue='getintensity')



; buttons for One point fit

buttonbase1 = widget_base(base, yoffset=10, xoffset=10, column=1) ;BASE for One point fit interface

label2=widget_label(buttonbase1, value= 'One point fit', /align_left)
label3=widget_label(buttonbase1, value='', ysize=10)
label4=widget_label(buttonbase1, value='Resolution')
label5=widget_label(buttonbase1, value='', ysize=5)

buttsize=100
HMIbutton = WIDGET_BUTTON(buttonbase1, $                              ;The button belongs to 'base'
          VALUE = 'Open HMI image',$                                ;The button label.
          UVALUE = 'HMI1', xsize=buttsize)                           ;The button's User Value.

field5 = CW_FIELD(buttonbase1,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
Scalebutton = WIDGET_BUTTON(buttonbase1,VALUE = 'Open unscaled image with limb',UVALUE = 'unscale1', xsize=buttsize)
field6 = CW_FIELD(buttonbase1,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
label3=widget_label(buttonbase1, value='', ysize=10)
label4=widget_label(buttonbase1, value='Select one point')
label5=widget_label(buttonbase1, value='', ysize=5)
firsbutton = WIDGET_BUTTON(buttonbase1,VALUE = 'Open first image',UVALUE = 'firstimage1', xsize=buttsize)
field7 = CW_FIELD(buttonbase1,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
secondbutton = WIDGET_BUTTON(buttonbase1,VALUE = 'Open second image',UVALUE = 'secondimage1', xsize=buttsize)
field8 = CW_FIELD(buttonbase1,  value='', TITLE = "File directory:", RETURN_EVENTS=1, XSIZE=50, noedit=1)
label6=widget_label(buttonbase1, value='', ysize=15)
coeffbutton = widget_button(buttonbase1, value= 'Determine coefficients', uvalue='coeff1')
label7=widget_label(buttonbase1, value='', ysize=5)
rotbutton=widget_button(buttonbase1, value='Rotation', uvalue= 'rotation1')
label8=widget_label(buttonbase1, value='', ysize=5)
resbutton=widget_button(buttonbase1, value='Resize', uvalue = 'resize1')
label9=widget_label(buttonbase1, value='', ysize=5)
transbutton=widget_button(buttonbase1, value='Translation', uvalue='translation1')
label10=widget_label(buttonbase1, value='', ysize=5)

slider = cw_fslider(buttonbase1, maximum=1, minimum=0, scroll=0.1, title='Set value of transparency', $
                      value=0, uvalue='a1')
label10=widget_label(buttonbase1, value='', ysize=5)
transparbutton=widget_button(buttonbase1, value='Transparency', uvalue='transparency1')
label11=widget_label(buttonbase1, value='', ysize=10)
getspecterbutton=widget_button(buttonbase1, value='Get intensity', uvalue='getintensity1')

label12=widget_label(buttonbase1, value='', ysize=10)
slitbutton=widget_button(buttonbase3, value='Slit rotate', uvalue='slitrotate', xsize=buttsize)
rad = widget_button(buttonbase3, value = 'Read radius file', UVALUE='radius', xsize=buttsize) ;read file with measured radiuses
coefs=widget_button(BUTTONBASE3, value='Read coefs', uvalue='coef', xsize=buttsize)
correlate=widget_button(BUTTONBASE3, value='Correlate images', uvalue='correlate', xsize=buttsize)

; Realize the widgets:
WIDGET_CONTROL, base, /REALIZE
; Hand off to the XMANAGER:
XMANAGER, 'fitsoverlap_r', base, GROUP_LEADER = GROUP, /NO_BLOCK


end