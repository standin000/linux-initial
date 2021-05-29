#!/bin/sh

echo "Starting backup for `date`\n"
# setup script variables
export BORG_PASSPHRASE="secret-passphrase-here!"
# export BORG_REPO="/mnt/backup/pilot-build"
#export BACKUP_TARGETS="~"
#export BACKUP_NAME="legend"
#export BACKUP_EXCLUDE="~/Trash"

export BORG_REPO=$1
export BACKUP_TARGETS=$2
export BACKUP_NAME=`basename $BACKUP_TARGETS`
export BACKUP_EXCLUDE=$3

umount /mnt/backup
mount /mnt/backup

# create borg backup archive
cmd="$HOME/.local/bin/borg create -v --stats ::$BACKUP_NAME`date +%Y%m%d`  $BACKUP_TARGETS --exclude $BACKUP_EXCLUDE"

echo $cmd

$cmd

# prune old archives to keep disk space in check
#borg prune -v --list --keep-daily=3 --keep-weekly=2
$HOME/.local/bin/borg prune -v --list --keep-within=90d

# pilot,2020/09/15: crontab don't support `date +%Y%m%d`
# ~/linux-initial/shell/borg_backup.sh /mnt/backup/pilot-build /home/pilot /home/pilot/Trash >> /home/pilot/tmp/borg.log 2>&1

# sync backups to offsite storage
# b2 authorize-account accountID applictionKey
# b2 sync --delete --replaceNewer $BORG_REPO b2://bucket-name/$BACKUP_NAME

# all done!
echo "Backup complete at `date`\n";
