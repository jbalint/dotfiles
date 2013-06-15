-- -*- indent-tabs-mode: nil -*-
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

imap_account = oracle
inbox = imap_account["INBOX"]

personal = {destination="followup",
            sent_to={"jess.balint@oracle.com"},
            other_rules={Mailbox.is_flagged}}
connectors = {destination="connectors",
              sent_to={"mysql-connectors_ww@oracle.com",
                       "mysql-connectors_ww_grp@oracle.com",
                       "mysql-connectors-java_grp@oracle.com"}}
dev_bugs = {destination="dev-bugs",
            sent_to={"mysql_dev-bugs_ww@oracle.com"}}
dev_private = {destination="dev-private",
               sent_to={"mysql_dev-private_ww_grp@oracle.com",
                        "MYSQL_DEV-PRIVATE_WW@oracle.com"}}
dev_commits = {destination="dev-commits",
               sent_to={"mysql_commits_ww_grp@oracle.com",
                        "mysql_commits_ww@oracle.com"}}
fabric = {destination="fabric",
          sent_to={"mysql_dev_ham_ww_grp@oracle.com"}}

-- in order of priority
--rules = {personal, connectors, dev_bugs, dev_private, dev_commits, fabric}
rules = {personal, connectors, dev_bugs, dev_private, dev_commits, fabric}

for i, rule in ipairs(rules) do
   local msgs = {}

   -- handle sent_to rules
   if not rule.sent_to then
      rule.sent_to = {}
   end
   for i, addr in ipairs(rule.sent_to) do
      msgs = msgs + inbox:contain_to(addr) + inbox:contain_cc(addr)
   end

   -- handle other_rules, pass mailbox directly, should return messages
   if not rule.other_rules then
      rule.other_rules = {}
   end
   for i, sub_rule in ipairs(rule.other_rules) do
      msgs = msgs + sub_rule(inbox)
   end
   msgs:move_messages(imap_account[rule.destination])
end

