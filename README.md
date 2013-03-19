Who is in the office?
====

This is an unfinished hubot plugin written in Haskell, C and Coffee-Script.

Operation:

* `whobot` reads list of devices from `people.yaml` file which looks like this:

        Hubot:
        - name: xnud.local.
        John Doe:
        - mac: 01:04:3F:0F:34:08
          name: MacBook
          type_: dhcp
        - name: JohnDoes-iPhone
        Jane Doe:
        - name: Jane-Does-iPhone
        - name: Janes-MacBook-Pro.local

* `whobot` uses IPv6 pings and Mikrotik RouterOS DHCP lease database to see what devices are on the local network

TODO
===

* more configurability
* support for false-positives (some guys tend to keep their MacBooks in the office and powered on)
* turn-key builds (i was lazy)
