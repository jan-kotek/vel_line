 PRO VEL_LINE,FILE=file, PRINT=print
;HEADER
; Based on curvefit (function prescription necessary) and voigt_fit
;17.04.2011 - input	: 	emission profile - array [[intensity],[wavelenght]]
;			- output: 	Parameters of  functions: 	Gaussian:G, max. intensity, lambda max.,HWHM, integrated intensity,dopp,CHI2
;												 	Lorentzian:L, max. intensity, lambda max.,HWHM, integrated intensity,dopp,CHI2
;												 	Voigt: V, max. intensity, lambda max., DOPP.P, DAMP.P,integrated intensity, dopp,chix
;										written to file nameout/parameters. par
;
;					  	graph: nameout/graphs/name.png


;===================== SETTINGS   ========================
;Select analysed line
;	lambda0=8542.144    ;CaII 8542
	lambda0=6562.808    ;H-alpha
;	lambda0=4861.339    ;H-beta
;name of output folder
	nameout=DIALOG_PICKFILE(/DIRECTORY,TITLE="Select output folder")
;..........................................................
hwc=0.1 ;parametr odchylky od ide�ln� y sou�adnice pr�se��ku konst. fce ymax/2 a emisn�ho profilu
	    ;p�i z�sk�n� x sou�adnice pot�ebn� pro v�po�et HWHM, z x sou�adnic p�i�azen�ch k y splnuj�c�m
	    ;podm�nku <ymax/2 +hwc*ymax and >ymax/2 -hwc*ymax  je spo��t�n jejich median (v p��pad� sud�ho po�tu pak prum�r dvou st�edn�ch hodnot)

;=========================================================

;********************************************************
;/HEADER
;********************************************************
!P.CHARSIZE=2.5
!P.FONT=1
filer=''
r=filer+pickfile(/MUST_EXIST,TITLE='select emission profile',FILTER='*.fts', $
              GET_PATH=pathk,    /READ)
print,r
print,'path= ', pathk
npath=STRLEN(pathk)
nfile=STRLEN(r)
npo=nfile-npath
print,npo
path1=STRMID(r,npath,npo)
print,'The file to be analyzed: ',path1
ncfs=STRLEN(path1)
path2=path1+' '
STRPUT,path2,'    ',ncfs-4

LOADCT,3

;OPENR,UNIT,r,/GET_LUN
array=FLTARR(2,16501)
x=FLTARR(16501)
c=FLTARR(16501)
;fint=c
OPENR,unit,r,/GET_LUN
image = readfits(r,header)
nx=sxpar(header,'NAXIS1')
;print, 'Number of points in scan ',r,':',nx
CLOSE,unit
FREE_LUN,unit
; displaying the two files
lam1=REVERSE(image(*,0))
int1=REVERSE(image(*,1))
help, image,lam1,int1
;print,nx
;nasto=1.0
count=nx
c=int1(0:count-1)  ;!!!
x=lam1(0:count-1)

d=array(*,0:count-1)
IF KEYWORD_SET(file) THEN BEGIN
 new_file=r+' '
 STRPUT,new_file,'g.fts',npath+ncfs-4
 new_file=STRCOMPRESS(new_file)
 print,'Cleaned intesities should be written into: ',new_file
;  writefits,new_file,d
ENDIF
int=INT_TABULATED(x,c)
ab=[c(0:count-1),c(0:count-1)*1.2,-0.2*c(0:count-1)]  ; !!!
maxy=MAX(ab)
miny=MIN(ab)
miny=miny-0.5*maxy
maxy=maxy+0.5*maxy

STRPUT,path2,'g',ncfs-5
path2=STRCOMPRESS(path2)
;x=x-lambda0
plot,x,c,yrange=[miny,maxy],xsty=1,ysty=1 ,$
  TITLE=path2+"   INTEGRATED INTENSITY= "+string(int,format='(f9.2)'),$
         XTITLE="Wavelength [Angstroms]", YTITLE="INTENSITY %" ,$
  ;BACKGROUND=0, COLOR=210,$
 CHARSIZE=1.0;,[XY]CHARSIZE=1.0
;oplot,x,c,LINESTYLE=0, COLOR=2
;yma=max(y)+.2*abs(max(y))&ymi=min(y)-.2*abs(min(y))
;plot,x,y,yrange=[ymi,yma],xsty=1,ysty=1
print,' Cursor the 1st noise level'&wait,.5
cursor,x1,y1&plots,x1,y1,psym=1&bell&wait,.5
print,' Cursor the 2nd noise level'&wait,.5
cursor,x2,y2&plots,x2,y2,psym=1&bell&wait,.5
print,x1,y1,x2,y2
oplot,[x1,x2],[y1,y2],LINESTYLE=4
; zde se pocitaji parametry primky y = que +  kon * x
kon=(y2-y1)/(x2-x1)
que=y1-kon*x1
zz=c
yz=c
;zz=c-(kon*x+que)
pocet=n_elements(x)
zero=0.0
;print,x1,x2
zz=where((x GE x1) AND (x LE x2),count1)
;yz= FLTARR(count1)
yz=c(zz(0):zz(count1-1))
xx=x(zz(0):zz(count1-1))
help,zz,xx,yz
pocet=n_elements(xx)
  FOR i=0, pocet-1 DO BEGIN
;  yz(i)=c(i)
  yz(i)=yz(i)-(kon*xx(i)+que)
 ; IF(yz(i) LT zero) THEN  yz(i)=zero
  ENDFOR
wdelete
int=INT_TABULATED(xx,yz)
print,int,'intenzita'
maxy=MAX(yz)+10
miny=MIN(yz)-10
on_error,2                      ;Return to caller if an error occurs
n = n_elements(yz)            ;# of points.
yma=max(yz)+10&ymi=min(yz)-10
plot,xx,yz,xrange=[x1-1,x2+1],yrange=[ymi,yma],xsty=1,ysty=1, $
 TITLE=path2,$ ; +"   INTEGRATED INTENSITY= "+string(int,format='(f9.2)')
         XTITLE="Wavelength [Angstroms]", YTITLE="INTENSITY %" ,$
  BACKGROUND='FFFFFF'x, COLOR='000000'x,thick=2,$
 CHARSIZE=1.5;,[XYZ]CHARSIZE=1.0
 common Pocet,no
 s=strarr(3)

oplot,[lambda0,lambda0],[miny,maxy],COLOR='000000'x,thick=1,line=1
;*************max intensity and HWHM derivation***********************************************************
parabol_max,xx,yz,xmax,ymax
yh=ymax/2
xxx=xx[where(xx LE xmax)]
yzz=yz[where(xx le xmax)]
U=[[xxx],[yzz]]
YHZ=MEDIAN(U[WHERE(U GE yh-hwc*ymax AND U LE yh+hwc*ymax)-1], /EVEN)
f=n_elements(yzz)
XH=MEDIAN(U[WHERE(U GE yh-hwc*ymax AND U LE yh+hwc*ymax)-f], /EVEN)
no=1
i=1
a=fltarr(3*no)
a(3*i-2)=xmax
a(3*i-3)=ymax
a(3*i-1)=xmax-xh
print,'hwhm',a(3*i-1)
print,'**************************CURVEFIT Lorentz*******************************************'
risulL=curvefit(xx,yz,replicate(1.,n),a,sigmaL,chisq=OL, $
            function_name = "LORENTZ1",/NODERIVATIVE) ;call curvefit
oplot,xx,risulL,line=2,thick=2,COLOR='00ff00'x
PRINT,'A',A
PRINT,'SIGMAL',SIGMAL
print,'ol',ol
wait, 0
intf=INT_TABULATED(xx,risulL)
nint=fltarr(no)
maxmin=INDGEN(no+1)
maxmin(0)=0
mini=MIN(maxmin);(minip)
maxi=MAX(risulL);maxi=MAX(maxmin);(maxip)
w0=lambda0
dopp=fltarr(no)
for i=1,no do begin
dopp(i-1)=300000*(a(3*i-2)-w0)/w0
endfor
 ;-----------parameters--------------
    ;not working for i NE 1
 cd,nameout
 namef="parameters.par"
 if (file_test(namef) EQ 0 )then openw,unit,namef,WIDTH=240,/get_lun else openu,unit,namef,WIDTH=240,/APPEND,/get_lun
  PRINTF,unit,path1,' ',lambda0," L",a(3*1-3),a(3*1-2),a(3*1-1),INTF,dopp(1-1),OL
 close,unit
 ;-----------------------------------
 pom=''

oplot,[w0,w0],[mini,maxi],COLOR='000000'x,thick=2,line=1
print,'************************CURVEFIT GAUSS***********************************'
no=1
i=1
a=fltarr(3*no)
a(3*i-2)=xmax
a(3*i-3)=ymax
a(3*i-1)=xmax-xh
;print,'hwhm',a(3*i-1)

risulG=curvefit(xx,yz,replicate(1.,n),a,sigmaG,CHISQ=OG, $
            function_name = "GAUSS1") ;call curvefit
 oplot,xx,risulG,line=1,thick=4,COLOR='ff0000'x
 PRINT,'A',A
 PRINT,'SIGMAG',SIGMAG
 print,'og',og
;print,'risulG',risulG
wait, 0
intf=INT_TABULATED(xx,risulG)
nint=fltarr(no)
maxmin=INDGEN(no+1)
maxmin(0)=0
mini=MIN(maxmin);(minip)
maxi=MAX(risulG);maxi=MAX(maxmin);(maxip)
w0=lambda0
dopp=fltarr(no)
for i=1,no do begin
dopp(i-1)=300000*(a(3*i-2)-w0)/w0
endfor

 ;-----------parameters--------------
    ;not working for i NE 1

 cd,nameout
 namef="parameters.par"
 if (file_test(namef) EQ 0 )then openw,unit,namef,WIDTH=240,/get_lun else openu,WIDTH=240,unit,namef,/APPEND,/get_lun
   PRINTF,unit,path1,' ',lambda0," G",a(3*1-3),a(3*1-2),a(3*1-1),INTF,dopp(1-1),og
 close,unit
 ;-------------------------------------
 pom=''
print,'************************CURVEFIT VOIGT***********************************';i=1
no=1
i=1
a=fltarr(6*no)
a(0)=0
a(4)=xmax
a(3)=ymax
a(5)=xmax-xh
risulV=voigt_fit(xx,yz,a,sigmav,damp=damp,chi2=ov) ;call curvefit
 oplot,xx,risulV,line=0,thick=2,COLOR='0000ff'x
 PRINT,'A',A
 PRINT,'SIGMAV',SIGMAG
 print,"ov",ov,'ol',ol,'og',og
 print,'results: ','doppler ',a(5),'damping ',a(6)
print,damp
wait, 0
intf=INT_TABULATED(xx,risulv)
nint=fltarr(no)
maxmin=INDGEN(no+1)
maxmin(0)=0
mini=MIN(maxmin);(minip)
maxi=MAX(risulv);maxi=MAX(maxmin);(maxip)
w0=lambda0
;wa=fltarr(no)
dopp=fltarr(no)
for i=1,no do begin
dopp(i-1)=300000*(a(4)-w0)/w0
endfor
 ;-----------parameters--------------
    ;not working for i NE 1
 cd,nameout
 namef="parameters.par"
 if (file_test(namef) EQ 0 )then openw,unit,namef,WIDTH=240,/get_lun else openu,WIDTH=240,unit,namef,/APPEND,/get_lun
   PRINTF,unit,path1,' ',lambda0," V",maxi,a(4),a(5),a(6),INTF,DOPP(0),ov
 close,unit
 ;-------------------------------------
;Result:
PRINT,'reduced chi-square goodness-of-fit statistic of LORENTZIAN & GAUSS',OL,OG
IF OL LE OG THEN PRINT,'LORENTZIAN SHAPE FITS BETTER' ELSE PRINT,'GAUSSIAN SHAPE FITS BETTER'
;-------------graphs----------------
CD,nameout
 if (file_test("graphs",/DIRECTORY) EQ 0) then FILE_MKDIR,"graphs"
  FILE_MKDIR,"graphs"
  pathk2=nameout+"/graphs"
  cd,pathk2
  WRITE_png,path1+".png", TVRD(true=1)
END
