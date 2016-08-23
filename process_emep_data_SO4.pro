pro Emep_data_year_read,SPECIES,file,obs_year,data

; initialize 
read_data=0
start_line=0
stop_check=0
prev_day=''

;open the data file
openr, 1, file ; open file
  data = fltarr(366) ; define daily obs as 366 days
  data[*]=!VALUES.F_NAN

  tmp = '' ; initiate the tmp variable
  i = 0 ; initiate the index for the loop

  while (not EOF(1)) do begin
    ; Read one value at a time until EOF
    readf, 1, tmp ; read from file

    ;now we are in the data part of the file = set read_data =1
    if(read_data gt 0) then read_data=read_data+1

    ;split the string and find the start of the data
    find_data=strsplit(tmp,' ',/extract)
    if(find_data[0] eq 'starttime') then read_data=read_data+1
 
    ;find the element that contains the data
    if(read_data eq 1) then begin 
      if(SPECIES eq 'TOTSO4') THEN BEGIN
        data_el=where(find_data eq 'value')
        ; if value does not work then search for SO4 = std sulfate
        if(data_el[0] lt 0) then data_el=where(find_data eq 'SO4--')
        if(n_elements(data_el) gt 1) then stop
        if(data_el[0] lt 0) then print,'cannot find sulfate element in data file :',file
        numflag_el=where(find_data eq 'flag_SO4--')
      endif ; TOTSO4

      ; USE FOR corrected SO4 DATA READ
      if(SPECIES eq 'XSO4') THEN BEGIN
        data_el=where(find_data eq 'value')
        if(data_el[0] lt 0) then data_el=where(find_data eq 'XSO4--')
        if(n_elements(data_el) gt 1) then stop
        if(data_el[0] lt 0) then print,'cannot find corrected sulfate element in data file :',file
        numflag_el=where(find_data eq 'flag_XSO4--')
      endif ;xso4

        ; find the correct numflag to use
        if(numflag_el[0] lt 0) then numflag_el=where(find_data eq 'numflag')
        ;find the numflag element
        nfc=0
        while ( n_elements(numflag_el) gt 1) do begin
          nfc = nfc+1
          if(find_data[data_el[0]+nfc] eq 'numflag') then numflag_el=data_el[0]+nfc
        endwhile
        if(n_elements(numflag_el) gt 1) then stop
        if(numflag_el[0] lt 0) then print,'cannot find numflag element in data file :',file

        ;print,data_el,numflag_el
    endif ; read_data

    ;;; - use numflag to ignore values that are not valid
    if(read_data ge 2) then begin
       if(find_data[numflag_el] lt 0.01) then  data[read_data-2]=find_data[data_el] 
       ;;print,find_data,' ', data[read_data-2]
    endif

endwhile ; not end of file

close,1 ; close the data file

; set nans in data to nans in data arr
find_neg=where(data lt 0.0) 
data(find_neg)=!values.f_nan

; find the zeros and set them to nans
find_zero=where(data eq 0.0)
data[find_zero]=!VALUES.F_NAN

end

;------------------------------------------------------------------------------------------------------------
PRO get_station_info,lat,lon,alt,station_name,station_code

; read the file that contains the station information = name/code/lon/lat/alt
stat_info_file='/home/tbreider/DATA/EMEP/a_stations.dat'
OPENR,2,stat_info_file

nstations=231 
lon=fltarr(nstations) & lat=lon & alt=lon & station_name=strarr(nstations) & station_code=strarr(nstations)

tmp_stat=''
nskip=''
READF,2,nskip

FOR i=0,nstations-1 DO BEGIN
   tmp=''
   READF,2,tmp
   tmp_stat=STRSPLIT(tmp,/extract)
   if(n_elements(tmp_stat) eq 9 ) then j=0
   if(n_elements(tmp_stat) eq 10) then j=1
   if(n_elements(tmp_stat) eq 11) then j=2
     lat[i]     =FLOAT(tmp_stat[2+j])
     lon[i]     =FLOAT(tmp_stat[5+j])
     alt[i]     =FLOAT(tmp_stat[8+j])
     station_name[i]=tmp_stat(1+j)
     station_code[i]=tmp_stat(0)
     lon_check  =tmp_stat(7+j)
ENDFOR

CLOSE,2

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN CODE STARTS HERE
;------------------------------------------------------------------------------------------
pro process_emep_data_SO4
;
;  This code will read in daily observations of nss-sulfate and total sulfate in air from the
;  European Monitoring and Evaluation Programme (EMEP). The code can be used to output all 
;  stations in the network or just specified stations for selected years) 
;
;  The data can be downloaded from http://ebas.nilu.no
;
;  Written by Tom Breider June 2012. 
;
;  Inputs
;  ------
;  species            : Either XSO4 or TOTSO4 = corrected(nss-so4) or total sulfate 
;  pick_station_codes : Array of station codes for the stations that you want (Default is all = [*]) 
;  data_years         : Array of years that you want (Default is 1970-2015)
;
;  Outputs 
;  -------
;  observations_daily        : daily sulfate concentrations in ugS/m3 [station,day,year]
;  observations_daily_contin : daily sulfate concentrations in ugS/m3 [station,day]
;
;  Output units are the same as in the input data files = micro grams S / m3) 
;
;  Notes
;  -----
;  Make sure that the location of the data files and the emep list file is linked to correctly
;
;=============================================================================================

;;; Key Inputs 
;species = 'XSO4' ; corrected sulfate in surface air (corrected = sea salt sulfate has been removed)
species = 'TOTSO4'

;specify the stations that you want data for (eg. pick_station_codes=['AT0002R','NO0042G'] = see emep_station_codes )
pick_station_codes=['*']  ; use ['*'] for all stations in EMEP list file
;;pick_station_codes=[AT0002R,NO0042G]

;specify the data years that you want (eg. data_years=[2009,2010])

data_years=[1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Get the station name, code and lat/lon/alt info
get_station_info,lats,lons,alts,emep_station_names,emep_station_codes

  ; loc='/home/tbreider/DATA/EMEP_1980_2013/SO4_CORRECTED_AIR/'
  ; spawn,'ls /home/tbreider/DATA/EMEP_1980_2013/SO4_CORRECTED_AIR/',tmp
  loc='/home/tbreider/DATA/EMEP_1980_2013/TOMS_IDL_CODES/DATA_FILES/'
  spawn,'ls /home/tbreider/DATA/EMEP_1980_2013/TOMS_IDL_CODES/DATA_FILES/',tmp
   files=tmp(0:n_elements(tmp)-1)

   site_count=1
   this_station_code='DUMMY' ; set the first station_code to a dummy value
   prev_station_code='DUMMY1'

   ; set station names = station names that we want
   station_codes=string(pick_station_codes)
   if(pick_station_codes[0] eq '*') then station_codes=emep_station_codes

   ;declare the obs array and initialize to Nans
   observations_daily=fltarr(n_elements(station_codes),366,n_elements(data_years)) ; num_stations,ndays,nyears)
   observations_daily_contin=fltarr(n_elements(station_codes),366*n_elements(data_years)) ; continuous array of daily data over multiple yrs
   observations_daily[*,*,*]=!VALUES.F_NAN & observations_daily_contin[*,*]=!VALUES.F_NAN
   

  ; loop over the files in the data directory
  FOR i=0,n_elements(files)-2 DO BEGIN
   ;print,'reading file ....',files(i)
   file=loc+files(i)

     ;stations have mutiple files for individual years so use prev_station_code to check if we are reading a new station 
     prev_station_code=this_station_code

     ; split the file name and pull out the station name
     tmp1=STRSPLIT(file,'.',/extract)
     tmp2=STRSPLIT(tmp1[0],'/',/extract)   
     this_station_code=tmp2[n_elements(tmp2)-1]

     ;find the element of the EMEP station that matches this station
      station_el=where(emep_station_codes eq this_station_code)

     ; error check = print if the station_code is not found 
     if(station_el[0] lt 0.0) then print,'station_code :',this_station_code ,' is not found in EMEP list of stations' 
     if(station_el[0] lt 0.0) then print,'You should add this to the station list file'
     ;if(station_el[0] lt 0.0) then stop
   
     ; if new station = reset site_count 
     if(prev_station_code ne this_station_code) then site_count=0
     if((site_count ne 1) and (station_el[0] ge 0)) then print,'station_code :',this_station_code,' has element ', station_el[0],' in EMEP list'
     if(station_el[0] ge 0) then site_count=1
        
     ; find year of obs in the file name
     obs_year=float(strmid(tmp1[1],0,4)) ; get the sub-string of the first 4 chars
     this_obs_year=where(obs_year eq data_years) ; find ovs year element in our observation period
 
     ;store the data for the stations and years that are required
     pick_station_code_el = where(this_station_code eq station_codes)
     if(pick_station_code_el ne -1) then begin
       if(this_obs_year ne -1) then begin  ; do we want the observation year
         ; read the data from the file and year
         Emep_data_year_read,species,file,obs_year,data
         ; put the obs into a storage array
         if(station_el ne -1) then observations_daily       [pick_station_code_el,*,this_obs_year]=data[*] 
         if(station_el ne -1) then observations_daily_contin[pick_station_code_el,(this_obs_year*366):((this_obs_year+1)*366)-1]=data[*]
       endif
     endif 

 ENDFOR ; n_els(files) 

; Save out the updated file
SAVE,file='EMEP_sulfate_air_observations_1980_2010.dat',observations_daily,observations_daily_contin


stop
end
