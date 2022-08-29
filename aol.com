(base) ed@ed-B450M-DS3H:~$ telnet smtp.aol.com 587
Trying 66.163.170.44...
Connected to smtp.aol.g03.yahoodns.net.
Escape character is '^]'.
220 smtp.mail.yahoo.com ESMTP ready
HELP
500 5.5.1 Command HELP unrecognized.
QUIT
221 Service Closing transmission
