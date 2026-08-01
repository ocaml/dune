for file in "$@"; do
	if [[ "${file##*/}" != "AGENTS.md" ]]; then
		echo .md files are not allowed in this directory
		echo Please categorize your md files according to type of change
		exit 1
	fi
done
