
#TODO

* Adding pretty-print

* Adding rpc connection between client and server
	* json formatting
	* tcp connection

* Adding p2p connection between peers
	* json formatting
	* tcp connection

* Adding storage with sqlite
	* Storage of the current blockchain
	* Storage of my balance
	* Storage of the mempool of transactions ?
	
* Adding wallet
	* Cleaning code
	* Store the private key in a file

#NICE TO HAVE

* Test suite

* Reformat wallet to have a big private_to_address function

* Cypher the file where the private key is stored (with a password)

* Adding gossip protocol for message propagation

* Adding Merkle tree storage

* Adding repl when connecting to node

* HD wallet

#DONE

* ECDSA cryptography
	* Compressed keys
* Bitcoin format for pubkeys
	* others format for bitcoin keys

# Clean code

## Formatter
raco fmt -i --width 80 *.rkt

## Linter
raco review name-file.rkt

