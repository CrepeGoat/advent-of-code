use aoc_lib::utils::parsing_input;

#[derive(Debug, PartialEq, Eq)]
enum Bracket {
    Parenthesis,
    Square,
    Curly,
    Angle,
}

struct ErrorTracker {
    stack: Vec<Bracket>,
}

impl ErrorTracker {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn advance(&mut self, c: char) -> Result<Option<Bracket>, &'static str> {
        use Bracket::*;
        let (bracket, is_closing) = match c {
            '(' => Ok((Parenthesis, false)),
            '[' => Ok((Square, false)),
            '{' => Ok((Curly, false)),
            '<' => Ok((Angle, false)),

            ')' => Ok((Parenthesis, true)),
            ']' => Ok((Square, true)),
            '}' => Ok((Curly, true)),
            '>' => Ok((Angle, true)),

            _ => Err("invalid character"),
        }?;

        Ok(if is_closing {
            if let Some(open_bracket) = self.stack.pop() {
                (bracket != open_bracket).then(|| bracket)
            } else {
                Some(bracket)
            }
        } else {
            self.stack.push(bracket);
            None
        })
    }
}

fn syntax_score(s: &str) -> Result<(u64, u64), &'static str> {
    let mut error_tracker = ErrorTracker::new();

    let mut error_score = 0;
    for r in s.chars().map(|c| error_tracker.advance(c)) {
        use Bracket::*;
        error_score += match r? {
            Some(Parenthesis) => 3,
            Some(Square) => 57,
            Some(Curly) => 1197,
            Some(Angle) => 25137,
            None => 0,
        };
    }

    let autocomplete_score: u64 = error_tracker
        .stack
        .iter()
        .rev()
        .map(|b| {
            use Bracket::*;
            match b {
                Parenthesis => 1,
                Square => 2,
                Curly => 3,
                Angle => 4,
            }
        })
        .fold(0, |score, delta_pt| 5 * score + delta_pt);
    Ok((autocomplete_score, error_score))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syntax_score() {
        let sequence = vec![
            "[({(<(())[]>[[{[]{<()<>>",
            "[(()[<>])]({[<{<<[]>>(",
            "{([(<{}[<>[]}>{[]{[(<()>",
            "(((({<>}<{<{<>}{[]{[]{}",
            "[[<[([]))<([[{}[[()]]]",
            "[{[{({}]{}}([{[{{{}}([]",
            "{<[[]]>}<{[{[{[]{()[[[]",
            "[<(<(<(<{}))><([]([]()",
            "<{([([[(<>()){}]>(<<{{",
            "<{([{{}}[<[[[<>{}]]]>[]]",
        ];
        let scores = sequence
            .into_iter()
            .map(syntax_score)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let error_score = scores.iter().map(|&(_auto, error)| error).sum::<u64>();
        assert_eq!(error_score, 26397);

        let autocomplete_scores = scores
            .into_iter()
            .filter_map(|(auto, error)| (error == 0).then(|| auto))
            .collect::<Vec<_>>();
        assert_eq!(
            autocomplete_scores,
            vec![288957, 5566, 1480781, 995444, 294]
        );
    }
}

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input::<_, String>(stdin.lock());

    let scores = parsed_inputs
        .into_iter()
        .map(|s| syntax_score(&s))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let error_score = scores.iter().map(|&(_auto, error)| error).sum::<u64>();
    println!("error score: {:?}", error_score);

    let autocomplete_scores = scores
        .into_iter()
        .filter_map(|(auto, error)| (error == 0).then(|| auto))
        .collect::<std::collections::BinaryHeap<_>>()
        .into_sorted_vec();
    println!(
        "autocomplete count: {:?}",
        autocomplete_scores[autocomplete_scores.len() / 2]
    );
}
