-- https://github.com/lefcha/imapfilter/blob/master/samples/config.lua
---------------
--  Options  --
---------------
options.timeout = 120
options.subscribe = true


----------------
--  Accounts  --
----------------
oracle = IMAP {
    server = 'stbeehive.oracle.com',
    username = 'jess.balint@oracle.com',
    password = io.open("/home/jbalint/.oracle_sso_pw"):read("*l"),
    ssl = 'ssl3',
}

-----------------
--  Mailboxes  --
-----------------
-- in order of priority

inbox = oracle["INBOX"]

personal_msgs = inbox:contain_to("jess.balint@oracle.com") +
	      inbox:contain_cc("jess.balint@oracle.com")
personal_msgs:move_messages(oracle["followup"])

connectors_msgs = inbox:contain_to("mysql-connectors_ww@oracle.com") +
		inbox:contain_cc("mysql-connectors_ww@oracle.com") +
		inbox:contain_to("mysql-connectors_ww_grp@oracle.com") +
		inbox:contain_cc("mysql-connectors_ww_grp@oracle.com")
connectors_msgs:move_messages(oracle["connectors"])

dev_bugs_msgs = inbox:contain_to("mysql_dev-bugs_ww@oracle.com") +
	      inbox:contain_cc("mysql_dev-bugs_ww@oracle.com")
dev_bugs_msgs:move_messages(oracle["dev-bugs"])

dev_priv_msgs = inbox:contain_to("asd@oracle.com")
dev_priv_msgs:move_messages(oracle["dev-private"])

dev_commits_msgs = inbox:contain_to("mysql_commits_ww_grp@oracle.com") +
		 inbox:contain_cc("mysql_commits_ww_grp@oracle.com") +
		 inbox:contain_to("mysql_commits_ww@oracle.com") +
		 inbox:contain_cc("mysql_commits_ww@oracle.com")
dev_commits_msgs:move_messages(oracle["dev-commits"])

