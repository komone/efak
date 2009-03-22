#!/bin/sh
rm doc/*.html doc/*.css doc/*.png doc/edoc-info
rm ebin/*.beam ebin/bg_example/*.beam ebin/bg_example/issuer/LOGFILE ebin/bg_example/acquirer/LOGFILE
rm ebin/bg_example/issuer/Mnesia.issuer@mini/*
rmdir ebin/bg_example/issuer/Mnesia.issuer@mini/
rm ebin/bg_example/acquirer/Mnesia.acquirer@mini/*
rmdir ebin/bg_example/acquirer/Mnesia.acquirer@mini/

