This is a simple utility to estimate how busy an area is using wifi signals. I built this as a project for Dropbox's Hack Week 2015. My inspiration is waiting in long lines at the Tuck Shop (and wishing I wasn't).

It connects to a Kismet server to gather Wi-Fi data.

It processes this data and keeps track of all MAC addresses seen in the last minute, as well as their respective signal strengths.

It then presents this data over HTTP.

http://server/ shows a human-friendly signal strength indicator.

http://server/raw shows a list of RSSIs (without their associated MAC addresses).

To install,

    cd Dropline
    cabal install
