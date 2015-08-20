A simple utility to estimate how busy an area is using wifi signals. 
Built for Dropbox's Hack Week 2015. 
Inspired by long lines at the Tuck Shop.

It connects to a Kismet server to gather Wi-Fi data.

It processes this data and keeps track of all MAC addresses seen in the last 5 minutes, as well as their respective signal strengths.

It then presents this data over HTTP.

`http://server/` shows a human-friendly signal strength indicator.

`http://server/raw` shows a list of RSSIs and how long they've been visible (without their associated MAC addresses).

To install,

    cd Dropline
    cabal install
