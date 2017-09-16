# SubzBor  
![Image of Yaktocat](https://github.com/m-audio91/SubzBor/raw/master/extra/icon/80.png)  
v1.0.3

Free subtitle cutting tool, with the help of FFmpeg and MKVToolNix.

#### it supports the following formats:
Available out of the box:
* Subrip (srt)
* Advanced Substation Alpha (ass)
* Substation Alpha (ssa)
* Web Video Text Tracks Format (vtt)
* Synchronized Accessible Media Interchange (smi , sami)
* Spruce subtitle format (stl)

Available with MKVToolNix:
* VobSub (idx|sub)
* Presentation Graphic Stream (sup)

The output of all text based subtitles is SRT. and for image based ones is the same as their input.  

Copyright (C) 2017 Mohammadreza Bahrami, m.audio91 [AT] gmail.com  
  
#### compilation guide:  
1. clone this repository plus [CommonUtils](https://github.com/m-audio91/CommonUtils) and [CodecUtils](https://github.com/m-audio91/CodecUtils) repositories.
2. update your Free Pascal and Lazarus to at least the version mentioned in [latest SubzBor release](https://github.com/m-audio91/SubzBor/releases) description. use [fpcupdeluxe](https://github.com/newpascal/fpcupdeluxe) if you have problem updating.
3. open the project (subzbor.lpi) in Lazarus and go to `project > project options > compiler options > paths > Other unit files` to add *CommonUtils* and *CodecUtils* folders to your unit search paths.
4. compile and run.
5. issues? please report [here](https://github.com/m-audio91/SubzBor/issues)  
note: never compiled nor tested on MacOS. but you can help development for this platform too.



[**Download Latest Version**](https://github.com/m-audio91/SubzBor/releases/latest)
[**Screenshots**](https://github.com/m-audio91/SubzBor/tree/master/extra/screenshots)

