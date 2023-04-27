
#TODO

* Adding pretty-print

* Adding rpc connection between client and server
	* json formatting
	* tcp connection

* Adding p2p connection between peers
	* json formatting
	* tcp connection

* Adding storage with sqlite

* Adding wallet
	* Bitcoin format for pubkeys

* Adding gossip protocol for message propagation

* Adding Merkle tree storage

* Adding repl when connecting to node

DONE
	* ECDSA keys

# Clean code

## Formatter
raco fmt -i --width 80 *.rkt

## Linter
raco review name-file.rkt

