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
	The Guy Who Has a Windows Phone Device With No Host Name:
	- mac: de:ad:be:ef:f0:0d
	
* `whobot` uses IPv6 pings and Mikrotik RouterOS DHCP lease database to see what devices are on the local network

INSTALLATION
===

    make install
    ln -s $(pwd)/src/scripts/whobot.coffee /path/to/hubot/scripts/

ENVIRONMENT
===

* `PEOPLE_YAML` must point to `people.yaml` using a resolvable path (`./people.yaml` is used by default)
* `PING_INTERFACE` must point to the IPv6-capable network interface within a local network (`en1` is used by default)
* `whobot` and `ping6x` must be in `$PATH`

OPERATION RECOMMENDATIONS
===

* DHCP server lease time should be as little as possible to avoid bad results.
* Better identify using mobile phones rather than laptops.

LICENSE
===

Everything is public domain unless stated otherwise.

NOTES
===

Used only with GHC 7.4.2 and haskell-platform 2012.4.0.0 that comes in homebrew at the time of writing.
I still suck at managing cabal dependencies properly so pull requests are welcome.
