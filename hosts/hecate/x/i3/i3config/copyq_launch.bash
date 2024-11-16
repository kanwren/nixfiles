#! @runtimeShell@

export PATH=@copyq@/bin${PATH:+:$PATH}

while true; do
	copyq exit || true
	copyq
	sleep 5
done
