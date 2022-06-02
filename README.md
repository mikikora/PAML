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
* `Relation` *name* *list_of_properties* -- to declare new relation (separete properties with comma)
* *Relation_name* *list_of_properties* -- to add properties of existsing relation
* *Relation_name* `unset` *list_of_properties* -- to remove properties of existsing relation
* `Save` *path_to_file* -- to save progress (can be used in proving mode)
* `Load` *path_to_file* -- to restore save from given file
* `Generate` *path_to_file* -- to generate LaTeX file with proved theorems
* `Theorem` *name* `with` *relation_name*, *judgement to prove* -- start proving given judgement. After this command program enters proving mode.

Note that if there is any theorem proven with a relation this relation properties can no longer be changed.

### Proving mode
In this mode program can be in focused or unfocused mode. In unfocused mode user can see open goals. With `focus` *number of goal* user can focus on given goal (if number not provided first goal will be chosen). User starts in this mode and can focus with command `Proof` (only if starting proof). If there are no more open goals `qed` command will close prove and return to declaring mode. 

In focused mode user can see all assumptions in local context and judgement to prove. If hints were turned on then they will be visible in this mode. Command avaible in this mode:
* `unfocus` -- to enter unfocus mode
* `Apply` -- to apply (use elimination rule) given judgement, one of the assumptions (by name) or already proved theorem in global context (also by name). To give world name add `with` *world* (seperate world with comma). To name any newly generated assumption use `as` *name*
* `Intro` *optional_name* -- to intro (use introduction rule). To give world name add `with` *world*.
* *property* `with` *worlds* to use rule associated with relation property. World should be seperated with comma. To give name to new assumption use `as` *name*. 
* `undo` -- to undo last command.
* `contra with` *world* -- to get false judgement in world *world* to prove insted of current goal
* `assumption` -- will try to close current goal with assumption in context
* `try ` *tactic* -- will try use *tactic* and if it would fail insted does nothing
* *tactic1*`;`*tactic2* -- will use *tactic1* on current goal and then *tactic2* on all subgoals that *tactic1* had created. 
* `auto ` -- optionally with number of application possible (default 5) will try to close current goal by first using introduction rules as much as it is possible and then trying apply from current context. **Warning:** `auto` can't be used in `try` or `;` as it is not treated as tactic.
* `hints on/off` -- to turn on/off hints in this mode. This command can be used in every mode of the program, but effect can be seen only in this mode.

In both modes *save* and *undo* commands are avaiable.

## Aditional notes
Program recognizes OCaml style comments.

Syntax of judgements is similar to that of Coq. Avaible logical operators:
* "->" -- implication
* "/\\" -- conjunction
* "\\/" -- alternative
* "[]" -- box
* "<>" -- diamond
* "F" or "\_|\_" -- bottom

Full judgement example: "x : [](A -> B) -> ([]A -> []B)".

## Author
Mikołaj Korobczak
Wrocław 2022.
