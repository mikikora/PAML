# Prover to create proofs in modal logics
Interactive proover for modal logics

## Compilation
Requirements: 
* ocamlc >= 4.11
* menhir >= 20220210
* ocamllex >= 4.11
To compile simply run `make`. To run program run generated `modal_prover`. To make using application more pleasant you can run it with `rlwrap`. `make run` command will compile it and run with `rlwrap`.

## Usage
Programm has two modes: declaring and proving. Each command must end with dot to be executed.
### Declaring mode
Immidiatly after starting programm user will be in declaring mode. In this mode user can type one of the following commends:
* `Relation` *name* *list of properties* -- to declare new relation (separete properties with comma)
* *Relation name* *list of properties* -- to add properties of existsing relation
* *Relation name* `unset` *list of properties* -- to remove properties of existsing relation
* `Save` *path to file* -- to save progress (can be used in proving mode)
* `Load` *path to file* -- to restore save from given file
* `Generate` *path to file* -- to generate LaTeX file with proved theorems
* `Theorem` *name* `with` *relation name*, *judgement to prove* -- start proving given judgement. After this command program enters proving mode.

Note that if there is any theorem proven with a relation this relation properties can no longer be changed.

### Proving mode
In this mode program can be in focused or unfocused mode. In unfocused mode user can see open goals. With `focus` *number of goal* user can focus on given goal (if number not provided first goal will be chosen). User starts in this mode and can focus with command `Proof` (only if starting proof). If there are no more open goals `qed` command will close prove and return to declaring mode. 

In focused mode user can see all assumptions in local context and judgement to prove. Command avaible in this mode:
* `unfocus` -- to enter unfocus mode
* `Apply` -- to apply (use elimination rule) given judgement, one of the assumptions (by name) or already proved theorem in global context (also by name). To give world name add `with` *world* (seperate world with comma). To name any newly generated assumption use `as` *name*
* `Intro` *optional name* -- to intro (use introduction rule). To give world name add `with` *world*.
* *property* `with` *worlds* to use rule associated with relation property. World should be seperated with comma. To give name to new assumption use `as` *name*. 
* `undo` -- to undo last command.

In both modes *save* and *undo* commands are avaiable.

## Aditional notes
Program recognizes OCaml style comments.

Syntax of judgements is similar to that of Coq. Avaible logical operators:
* "->" -- implication
* "/\" -- conjunction
* "\/" -- alternative
* "[]" -- box
* "<>" -- diamond
* "F" or "_|_" -- bottom

Full judgement example: "x : [](A -> B) -> ([]A -> []B)".

## Author
Mikołaj Korobczak
Wrocław 2022.
