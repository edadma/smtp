(base) ed@ed-B450M-DS3H:~$ telnet smtp.mail.com 587
Trying 74.208.5.15...
Connected to smtp.mail.com.
Escape character is '^]'.
220 mail.com (mrgmxus005) Nemesis ESMTP Service ready
HELP
214-This server supports the following commands:
214-EHLO HELO MAIL RCPT DATA RSET NOOP QUIT HELP
214 STARTTLS AUTH
QUIT
221 mail.com Service closing transmission channel
