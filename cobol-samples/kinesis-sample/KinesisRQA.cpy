       01  kinesis-request-area.
           05 command PIC X(7).
             88 publish-command value 'PUBLISH'.
             88 read-command value 'READ'.
             88 create-command value 'CREATE'.
             88 delete-command value 'DELETE'.
           05 aws-region PIC X(24).
           05 stream-name PIC X(255).
           05 shard-count PIC 9(3) COMP-5 value 1.
           05 partition-key PIC X(255) value 'default'.
