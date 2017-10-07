# SubzBor  
![Image of SubzBor](https://github.com/m-audio91/SubzBor/raw/master/extra/icon/80.png)  
v1.4.5

SubzBor is a Graphical User Interface that uses FFmpeg, MKVmerge, MKVextract and it's own internal codecs to cut (split) subtitles.  
It's main purpose is to cut subtitles and keep them in sync while their movie is being edited in any frame accurate video editor that does not support subtitle processing.  
so there is no need to HardSub (burn in) nor any other complicated manual editing.  


#### it supports the following formats:
Available with internal codecs (ouput format = input format):
* Subrip (srt)
* Advanced Substation Alpha (ass)
* Substation Alpha (ssa)

Available with FFmpeg (ouput format = srt):
* Subrip (srt)
* Advanced Substation Alpha (ass)
* Substation Alpha (ssa)
* Web Video Text Tracks Format (vtt)
* Synchronized Accessible Media Interchange (smi , sami)
* Spruce subtitle format (stl)
* JACOsub subtitle (jss)
* MicroDVD subtitle (sub)
* 3GPP Timed Text subtitle (ttxt)
* MPL2 subtitle (mpl)
* PJS subtitle (pjs)
* RealText subtitle (rt)
* SubViewer subtitle (sub)
* SubViewer1 subtitle (sub)
* VPlayer subtitle (txt)

Available with MKVToolNix (ouput format = input format):
* VobSub (idx|sub)
* Presentation Graphic Stream (sup)

Copyright (C) 2017 Mohammadreza Bahrami, m.audio91 [AT] gmail.com  
  
#### compilation guide:  
1. clone this repository plus [CommonUtils](https://github.com/m-audio91/CommonUtils) and [CodecUtils](https://github.com/m-audio91/CodecUtils) repositories.
2. update your Free Pascal and Lazarus to at least the version mentioned in [latest SubzBor release](https://github.com/m-audio91/SubzBor/releases) description. use [fpcupdeluxe](https://github.com/newpascal/fpcupdeluxe) if you have problem updating.
3. open the project (subzbor.lpi) in Lazarus and go to `project > project options > compiler options > paths > Other unit files` to add *CommonUtils* and *CodecUtils* folders to your unit search paths.
4. compile and run.
5. issues? please report [here](https://github.com/m-audio91/SubzBor/issues)  
you also need to copy "Tools" folder from the latest SubzBor release next to your newly compiled executable. otherwise you can't split subtitles.  
note: never compiled nor tested on MacOS. but you can help development for this platform too.  



[**Download Latest Version**](releases/latest)  
[**Screenshots**](tree/master/extra/screenshots)  
[**Tutrial and guides**](wiki)

