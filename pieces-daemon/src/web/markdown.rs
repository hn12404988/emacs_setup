//! Markdown → HTML rendering for piece bodies. GitHub-flavored, server-side,
//! via comrak. Raw embedded HTML is neutralized (comrak's safe default).

use comrak::{Options, markdown_to_html};
use maud::{Markup, PreEscaped};

/// Render GFM `src` to HTML markup, safe to inject into a maud template.
pub fn render(src: &str) -> Markup {
    let mut options = Options::default();
    options.extension.table = true;
    options.extension.strikethrough = true;
    options.extension.tasklist = true;
    options.extension.autolink = true;
    options.render.r#unsafe = false;
    PreEscaped(markdown_to_html(src, &options))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn renders_heading_and_code() {
        let html = render("# Title\n\nUse `useState` here.").into_string();
        assert!(html.contains("<h1>"));
        assert!(html.contains("<code>useState</code>"));
    }

    #[test]
    fn raw_html_is_not_passed_through_as_live_tag() {
        let html = render("<script>alert(1)</script>").into_string();
        assert!(!html.contains("<script>"));
    }
}
