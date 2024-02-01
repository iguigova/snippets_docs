  # Prerequisites
        
- install [Java](https://www.oracle.com/java/technologies/downloads/)
- install [clojure](https://clojure.org/guides/install_clojure)
- install [leiningen](https://leiningen.org/)        
- install [mvn](https://maven.apache.org/download.cgi)       
- install [gpg](https://gnupg.org/download/)
- install [node.js/npm](https://nodejs.org/en/download)

*note: depending on OS, installers, and what not, consider adding the above exes to your env `PATH` variable*

# The Editor
        
- install [Emacs](https://www.gnu.org/software/emacs/download.html)
  - some sample init files can be found in the docs folder
  - TODO: add information about cider extension / cheat sheet / etc.

# The Repo
        
- install [git](https://git-scm.com/downloads)
  - a sample cheat sheet and a set of aliases can be found [here](https://github.com/iguigova/snippets_docs/blob/master/git.cheatsheet)
- create SSH keys: *example: [instructions](https://www.atlassian.com/git/tutorials/git-ssh)*
- share your public key with Scott: *example: `~/.ssh/id_ed25519.pub`*
  
- `git clone [username]@3.88.22.153:/aeonic` where [username] has been assign to you by Scott

let `[AEONIC]` be the folder where the repo is cloned to.
        
*note: `~` = `c:/Users/[your-username]` on Windows*

# Running the client / clojurescript
- `cd [AEONIC]/app`
- `npm install`
- `npx babel src/js --out-dir src/gen --watch --extensions '.ts,.tsx,.js'`
- `npm run watch`
```
shadow-cljs - HTTP server available at (http://localhost:8280)[http://localhost:8280]
shadow-cljs - HTTP server available at (http://localhost:8290)[http://localhost:8290]
shadow-cljs - server version: 2.19.9 running at (http://localhost:9630)[http://localhost:9630]
shadow-cljs - nREPL server started on port 8777
shadow-cljs - watching build :app
```

## Troubleshooting
### datomic credentials

- create [gpg keys](https://github.com/technomancy/leiningen/blob/stable/doc/GPG.md)
  - `gpg --gen-key`
  - `gpg --list-keys`              
- create an account at [datomic](https://my.datomic.com/)
  - copy the credentials at the bottom of the [page](https://my.datomic.com/)
```
;; ~/.lein/credentials.clj.gpg (see the Leiningen deploy authentication docs)
{#"my\.datomic\.com" {:username "ilka@invisiblerobot.ai"
                      :password "aaa9012b-c605-4c74-9050-5f775bdc0b38"}}
```
- `cd ~/.lein`
- create `credentials.clj` 
  - copy the credentials into it
  - [sign] it: (https://github.com/technomancy/leiningen/blob/master/doc/DEPLOY.md#authentication) `gpg --default-recipient-self -e ~/.lein/credentials.clj > ~/.lein/credentials.clj.gpg`
        
### loom
- find the enclosed loom folder: the pom file and the jar file
- `mvn install:install-file -Dfile="1.0.3-SNAPSHOT.jar" -DgroupId="aysylu" -DartifactId="loom" -Dversion="1.0.3-SNAPSHOT" -Dpackaging="jar"`

*note: your maven repository is at `~/.m2`*
        
# The REPL


# The Database
- install [Datomic Pro](https://docs.datomic.com/pro/getting-started/get-datomic.html)

let [DATOMIC] be the folder where the installation was unzipped to

- read [local dev setup](https://docs.datomic.com/pro/getting-started/dev-setup.html)
- read [run a transactor](https://docs.datomic.com/pro/getting-started/transactor.html)
- read [connect to a database](https://docs.datomic.com/pro/getting-started/connect-to-a-database.html)
- read [restore database](https://docs.datomic.com/pro/operation/backup.html#restoring)

- locate and unzip the backup db

let [KREWZ] be the folder that contains the backup file

- `[DATOMIC]/bin/datomic -Xmx4g -Xms4g restore-db file:[KREWZ "datomic:dev://localhost:4334/krewz"`

in the repl: 
```
(def krewz-uri "datomic:dev://localhost:4334/krewz")
(def krewz-conn (d/connect krewz-uri))
(def db (d/db krewz-conn))
(def all-labourers '[:find ?e
          :where [?e :laborer/name]])
(d/q all-labourers db)
```
## Troubleshooting
