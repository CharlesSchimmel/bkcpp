# bkcpp

![](bkcpp.png)

## ncmpcpp:mpd::bkcpp:kodi

Brick Kodi Client Plus Plus

The goal of this application is to be feature comparable to ncmpcpp.
Playlist/now playing view, video library browser, music browser.

This application is very much in development.

Feel free to open issues to comment on the code or request features.  

### Installation
Requires stack and assumes a Linux environment.

```
git clone https://github.com/CharlesSchimmel/bkcpp
cd bkcpp
stack install
```

### Usage

```
Usage: bkcpp (-a|--ip-address ip) [-p|--port port] [--username user]
             [--password pass] ([-y|--youtube url] | [-f|--cast-file file])
             [-q|--queue]
  A TUI client for the Kodi Media Center.

Available options:
  -a,--ip-address ip       IP address of running Kodi Media Center
  -p,--port port           Port (default: 8080)
  --username user          Username
  --password pass          Password
  -y,--youtube url         YouTube url to cast
  -f,--cast-file file      File to cast
  -q,--queue               Add casted media to current playlist
  -h,--help                Show this help text
```

Keyboard movement is largely the same as the [default Kodi keyboard
controls](https://kodi.wiki/view/Keyboard_controls) with the exception of `hjkl`
being used for directional navigation.
Brick Kodi Client Plus Plus

