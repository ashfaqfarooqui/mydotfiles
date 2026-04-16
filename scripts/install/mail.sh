mkdir -p ~/mail/rise
mkdir -p ~/mail/personal

# Store mailbox.org password in the system keyring (libsecret / gnome-keyring).
# aerc's accounts.conf retrieves it via:
#   secret-tool lookup service mailbox.org username ashfaq.farooqui@mailbox.org
echo "Enter mailbox.org password for IMAP/SMTP:"
secret-tool store \
  --label="mailbox.org" \
  service mailbox.org \
  username ashfaq.farooqui@mailbox.org
