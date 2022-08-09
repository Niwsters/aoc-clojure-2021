#bb --nrepl-server $(cat .nrepl-port)
clj -M:nREPL -m nrepl.cmdline --port $(cat .nrepl-port)
