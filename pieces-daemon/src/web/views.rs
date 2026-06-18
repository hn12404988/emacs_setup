//! maud templates: a shared `layout` shell wraps three read-only pages —
//! thread list → response list → piece reader (client-side prev/next).

use crate::models::{PieceRow, ResponseRow, ThreadRow};
use crate::web::markdown;
use maud::{DOCTYPE, Markup, PreEscaped, html};

const APP_CSS: &str = "\
:root{color-scheme:dark}\
body{font-family:system-ui,sans-serif;max-width:64rem;margin:2rem auto;padding:0 1rem;line-height:1.6;color:#c9d1d9;background:#0d1117}\
a{color:#58a6ff;text-decoration:none}a:hover{text-decoration:underline}\
h1,h2,h3{color:#e6edf3}\
header.top{border-bottom:1px solid #30363d;padding-bottom:.6rem;margin-bottom:1rem}\
header.top a.brand{font-weight:600;font-size:1.1rem;color:#e6edf3}\
nav.crumbs{color:#8b949e;margin:.5rem 0 1.5rem;font-size:.9rem}\
ul.list{list-style:none;padding:0}ul.list li{padding:.6rem 0;border-bottom:1px solid #30363d}\
.thumb{color:#8b949e;font-size:.9rem}\
.markdown-body pre{background:#161b22;padding:.8rem;border-radius:6px;overflow:auto;border:1px solid #30363d}\
.markdown-body code{background:#161b22;padding:.1rem .3rem;border-radius:4px}\
.markdown-body pre code{background:none;padding:0;border:none}\
.marker{color:#8b949e;font-size:.9rem;margin-bottom:1rem}\
.reader-layout{display:flex;gap:1.5rem;align-items:flex-start}\
.piece-nav{flex:0 0 15rem;position:sticky;top:1rem;max-height:90vh;overflow:auto;border:1px solid #30363d;border-radius:8px;padding:.4rem}\
.reader-main{flex:1;min-width:0}\
.piece{display:none}.piece h2{margin-top:0}\
.nav-item{display:flex;gap:.5rem;align-items:baseline;padding:.4rem .6rem;border-radius:6px;color:#c9d1d9;cursor:pointer}\
.nav-item:hover{background:#161b22;text-decoration:none}\
.nav-item.active{background:#1f6feb;color:#fff}\
.nav-num{color:#8b949e;font-variant-numeric:tabular-nums;min-width:1.2rem;text-align:right}\
.nav-item.active .nav-num{color:#cdd9ff}\
.nav-title{flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}\
.nav{margin-top:1.5rem;display:flex;gap:1rem}\
.nav button{font-size:1rem;padding:.4rem .9rem;cursor:pointer;background:#21262d;color:#c9d1d9;border:1px solid #30363d;border-radius:6px}\
.nav button:hover{background:#30363d}";

const READER_JS: &str = "(function(){\
var pieces=document.querySelectorAll('.piece');\
var navItems=document.querySelectorAll('.nav-item');\
var total=pieces.length;var cur=0;\
var marker=document.getElementById('marker');\
function show(i){if(i<0||i>=total)return;\
pieces[cur].style.display='none';if(navItems[cur])navItems[cur].classList.remove('active');\
cur=i;\
pieces[cur].style.display='block';if(navItems[cur])navItems[cur].classList.add('active');\
marker.textContent='piece '+pieces[cur].getAttribute('data-idx')+' of '+total;}\
var p=document.getElementById('prev');var n=document.getElementById('next');\
if(p)p.addEventListener('click',function(){show(cur-1);});\
if(n)n.addEventListener('click',function(){show(cur+1);});\
navItems.forEach(function(el,idx){el.addEventListener('click',function(e){e.preventDefault();show(idx);});});\
document.addEventListener('keydown',function(e){\
if(e.key==='ArrowLeft')show(cur-1);if(e.key==='ArrowRight')show(cur+1);});\
if(total>0)show(0);})();";

fn layout(page_title: &str, body: Markup) -> Markup {
    html! {
        (DOCTYPE)
        html lang="en" {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                title { (page_title) " — pieces" }
                style { (PreEscaped(APP_CSS)) }
            }
            body {
                header.top { a.brand href="/" { "pieces" } }
                main { (body) }
            }
        }
    }
}

pub fn thread_list_page(threads: &[ThreadRow]) -> Markup {
    let body = html! {
        h1 { "Threads" }
        @if threads.is_empty() {
            p.thumb { "No threads yet." }
        } @else {
            ul.list {
                @for t in threads {
                    li {
                        a href=(format!("/t/{}", t.id)) { (t.id) }
                        span.thumb { " — " (t.response_count) " responses · " (t.last_activity) }
                    }
                }
            }
        }
    };
    layout("Threads", body)
}

pub fn response_list_page(thread_id: &str, responses: &[ResponseRow]) -> Markup {
    let body = html! {
        nav.crumbs { a href="/" { "all" } " › " (thread_id) }
        h1 { (thread_id) }
        @if responses.is_empty() {
            p.thumb { "No responses yet." }
        } @else {
            ul.list {
                @for r in responses {
                    li {
                        a href=(format!("/t/{}/r/{}", thread_id, r.id)) { (r.title) }
                        div.thumb { (r.thumbnail) " · " (r.created_at) }
                    }
                }
            }
        }
    };
    layout(thread_id, body)
}

pub fn reader_page(thread_id: &str, response: &ResponseRow, pieces: &[PieceRow]) -> Markup {
    let total = pieces.len();
    let body = html! {
        nav.crumbs {
            a href="/" { "all" } " › "
            a href=(format!("/t/{}", thread_id)) { (thread_id) } " › "
            (response.title)
        }
        div.reader-layout {
            nav.piece-nav {
                @for (i, p) in pieces.iter().enumerate() {
                    a.nav-item href="#" data-pos=(i) {
                        span.nav-num { (p.idx) }
                        span.nav-title {
                            @match &p.heading {
                                Some(h) => (h),
                                None => "(untitled)",
                            }
                        }
                    }
                }
            }
            div.reader-main {
                div.marker id="marker" {
                    @if pieces.is_empty() { "No pieces" }
                    @else { "piece " (pieces[0].idx) " of " (total) }
                }
                @for p in pieces {
                    div.piece data-idx=(p.idx) {
                        @match &p.heading {
                            Some(h) => h2 { (p.idx) ". " (h) },
                            None => h2 { "Piece " (p.idx) },
                        }
                        div.markdown-body { (markdown::render(&p.body_md)) }
                    }
                }
                div.nav {
                    button id="prev" { "← prev" }
                    button id="next" { "next →" }
                }
            }
        }
        script { (PreEscaped(READER_JS)) }
    };
    layout(&response.title, body)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn resp() -> ResponseRow {
        ResponseRow {
            id: "r1".into(),
            title: "How it works".into(),
            thumbnail: "preview".into(),
            created_at: "2026-06-18T00:00:00Z".into(),
        }
    }

    #[test]
    fn thread_list_shows_ids_and_links() {
        let threads = vec![ThreadRow {
            id: "proj-abc".into(),
            response_count: 3,
            last_activity: "2026-06-18T00:00:00Z".into(),
        }];
        let html = thread_list_page(&threads).into_string();
        assert!(html.contains("proj-abc"));
        assert!(html.contains("href=\"/t/proj-abc\""));
        assert!(html.contains("3 responses"));
    }

    #[test]
    fn reader_renders_markdown_and_marker_and_nav() {
        let pieces = vec![
            PieceRow { idx: 1, heading: Some("The problem".into()), body_md: "# Big\n\ntext".into() },
            PieceRow { idx: 2, heading: None, body_md: "use `x`".into() },
        ];
        let html = reader_page("proj-abc", &resp(), &pieces).into_string();
        assert!(html.contains("The problem"));
        assert!(html.contains("<h1>Big</h1>"));
        assert!(html.contains("<code>x</code>"));
        assert!(html.contains("piece 1 of 2"));
        assert!(html.contains("id=\"prev\""));
        assert!(html.contains("id=\"next\""));
        assert!(html.contains("data-idx=\"1\""));
        assert!(html.contains("data-idx=\"2\""));
        assert!(html.contains("1. The problem")); // heading now prefixed with its real idx
        // left vertical piece-nav: one clickable item per piece (jump-to)
        assert!(html.contains("piece-nav"));
        assert!(html.contains("class=\"nav-item\""));
        assert!(html.contains("data-pos=\"0\""));
        assert!(html.contains("data-pos=\"1\""));
        assert!(html.contains("(untitled)")); // piece 2 has no heading
    }
}
