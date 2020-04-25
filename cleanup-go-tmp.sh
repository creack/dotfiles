#!/usr/bin/env sh

\ls /tmp/ | \grep go-build | while read l; do rm -rf /tmp/$l; done
\ls /tmp/ | \grep 'go\..*\.mod' | while read l; do rm -f /tmp/$l; done
\ls /tmp/ | \grep 'go\..*\.sum' | while read l; do rm -f /tmp/$l; done
echo "Last run: $(date)" > /tmp/lastrun
