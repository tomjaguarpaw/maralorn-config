require ["vnd.dovecot.duplicate", "fileinto", "regex", "imap4flags", "mailbox"];

if header :regex "X-spamd-result" "st-ludwig-darmstadt.de" {
	keep;
}
elsif anyof (
	header :regex "X-spam" "^yes$",
	header :contains "From" "paypal@mail.paypal.de",
	header :contains "To" "lac@maralorn.de",
	header :contains "To" "malte@maralorn.de",
	header :contains "To" "maltemail@maralorn.de",
	header :contains "To" "mail@maralorn.de",
	header :contains "From" "promotion5@amazon.de")
{
	setflag "\\Seen";
	fileinto "Spam";
}
elsif anyof (
	duplicate
) {
	setflag "\\Seen";
	fileinto "Trash";
}
elsif anyof (
	address :domain :is "From" [
		"maralorn.de",
		"facebookmail.com"
	],
	address :domain :is "From" [
		"tickets.darmstadt.ccc.de"
	],
	header :contains "List-Id" [
		"hackspace.lists.darmstadt.ccc.de",
		"members.lists.darmstadt.ccc.de",
		"darmstadt.lists.darmstadt.ccc.de"
	]
)
{
	setflag "\\Seen";
}
elsif anyof (
	header :is "From" "vib@vereinsknowhow.de",
	address :domain :is "From" [
		"dpg-physik.de",
		"physikstudenten.de"
	],
	header :contains "List-Id" [
		"intern.lists.ccc.de",
		"erfa.lists.ccc.de",
		"ctf-announce.lists.cased.de",
		"ctf-team.lists.cased.de",
		"darmstadt-freifunk.net"
	],
	header :contains "Subject" "Moodle TU Darmstadt: Zusammenfassung des Forums"
)
{
	setflag "\\Seen";
	fileinto :create "Move.readlater";
}
elsif header :contains ["To", "From", "CC"] [
		"noc@karlshof.de",
		"karlshof@whm.stwda.de",
		"noc-karlshof@maralorn.de"
	]
{
	setflag "\\Seen";
	fileinto :create "Archiv.karlshof.noc";
}

elsif header :contains "List-Id" "chor.lists.tu-darmstadt.de" {
	setflag "\\Seen";
	fileinto :create "Archiv.tuchor";
}
elsif header :contains "From" "kdwachlin@web.de" {
	setflag "\\Seen";
	fileinto "Archiv.unsortiert";
}
elsif header :contains "Subject" "[VED-Wiki]" {
	setflag "\\Seen";
	fileinto :create "Archiv.ved.wiki";
} elsif header :regex "X-spamd-result" "[2-5]\.[0-9]\{2\} / 15\.00" {
	setflag "\\Seen";
}
