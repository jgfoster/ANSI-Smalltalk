"Boot an ANSI development image"

| list |
list := #(
	'ANSI Spec Parser'
).
ProgressDialog showModalWhile: [:progress |
	1 to: list size do: [:i |
		| name percent |
		name := list at: i.
		percent := (i * 100 / list size) asInteger.
		progress
			value: percent;
			text: 'Loading package: ' , name;
			yourself.
		PackageManager current install: 'sources\' , name , '.pax'.
	].
].
SessionManager current splashShowAtStartup: false.
SmalltalkSystemShell allInstances first 
	position: 20@50;
	yourself.
IdeaSpaceShell allInstances isEmpty ifTrue: [
	IdeaSpaceShell show
		absorbCard: SmalltalkWorkspaceDocument show;
		absorbCard: SystemBrowserShell show;
		position: 50@80; extent: 1200@750;
		yourself.
].
Debugger defaultExtent: 1000@600.
!
Transcript clear.
(Delay forMilliseconds: 200) wait.
Cursor wait showWhile: [
	SourceManager default compressSources.
	SessionManager current saveImage.
].
!
