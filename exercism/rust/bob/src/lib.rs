pub fn reply(message: &str) -> &str {
    let message = message.trim();
    let question = message.chars().last().map_or(false, |ch| ch == '?');
    let shouting = message.chars()
        .filter(|&c| c.is_alphabetic())
        .all(char::is_uppercase) && message.chars().any(char::is_alphabetic);
    if message.is_empty() {
        return "Fine. Be that way!";
    }
    match (shouting, question) {
        (true,  true)  => "Calm down, I know what I'm doing!",
        (true,  false) => "Whoa, chill out!",
        (false, true)  => "Sure.",
        _              => "Whatever.",
    }
}
