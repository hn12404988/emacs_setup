//! maud templates: a shared `layout` shell wraps three read-only pages —
//! thread list → response list → piece reader (client-side prev/next).

use crate::models::{PieceRow, ResponseRow, ThreadRow};
use crate::web::markdown;
use maud::{DOCTYPE, Markup, PreEscaped, html};

// matrix / hacker terminal theme: phosphor-green monospace on black, a
// digital-rain canvas behind everything, CRT scanlines + flicker on top.
const APP_CSS: &str = "\
:root{color-scheme:dark;--mx:#00ff41;--mx-hi:#aaffc4;--mx-soft:#3dff74;--mx-dim:rgba(0,255,65,.45)}\
*{box-sizing:border-box}\
html{background:#000}\
body{font-family:'JetBrains Mono',ui-monospace,'Cascadia Code','SF Mono',Menlo,Consolas,monospace;max-width:64rem;margin:2rem auto;padding:0 1.2rem;line-height:1.7;color:var(--mx-soft);background:transparent;position:relative;z-index:1}\
#matrix-rain{position:fixed;inset:0;width:100%;height:100%;z-index:0;opacity:.16;pointer-events:none}\
body::before{content:'';position:fixed;inset:0;pointer-events:none;z-index:9;background:repeating-linear-gradient(0deg,rgba(0,0,0,.22),rgba(0,0,0,.22) 1px,transparent 1px,transparent 3px);animation:flicker 4s infinite}\
@keyframes flicker{0%,100%{opacity:.45}48%{opacity:.5}50%{opacity:.62}52%{opacity:.5}}\
::selection{background:rgba(0,255,65,.3);color:#001b08}\
::-webkit-scrollbar{width:10px;height:10px}\
::-webkit-scrollbar-track{background:rgba(0,255,65,.04)}\
::-webkit-scrollbar-thumb{background:rgba(0,255,65,.3)}\
::-webkit-scrollbar-thumb:hover{background:rgba(0,255,65,.5)}\
a{color:var(--mx);text-decoration:none;text-shadow:0 0 6px rgba(0,255,65,.55)}\
a:hover{color:var(--mx-hi);text-shadow:0 0 10px rgba(0,255,65,.9);text-decoration:underline}\
h1,h2,h3{font-family:'Share Tech Mono',ui-monospace,monospace;color:#7dffa0;text-shadow:0 0 10px rgba(0,255,65,.55);letter-spacing:.5px}\
main>h1{text-transform:uppercase}main>h1::before{content:'> ';color:var(--mx);text-shadow:0 0 10px var(--mx)}\
header.top{display:flex;align-items:center;justify-content:space-between;gap:1rem;border-bottom:1px solid rgba(0,255,65,.35);padding-bottom:.7rem;margin-bottom:1.4rem;box-shadow:0 1px 14px rgba(0,255,65,.12)}\
header.top a.brand{font-weight:700;font-size:1.25rem;color:var(--mx-hi);text-shadow:0 0 12px rgba(0,255,65,.9);letter-spacing:3px;text-transform:uppercase}\
header.top a.brand::before{content:'// ';color:rgba(0,255,65,.55)}\
header.top button.fs{flex:0 0 auto;display:inline-flex;align-items:center;justify-content:center;width:2.3rem;height:2.3rem;padding:0;cursor:pointer;background:rgba(0,255,65,.06);color:var(--mx);border:1px solid rgba(0,255,65,.4);border-radius:3px;box-shadow:0 0 10px rgba(0,255,65,.18);transition:background .15s,box-shadow .15s,color .15s}\
header.top button.fs:hover{background:rgba(0,255,65,.18);color:var(--mx-hi);box-shadow:0 0 16px rgba(0,255,65,.45)}\
header.top button.fs:active{transform:translateY(1px)}\
header.top button.fs svg{display:block;filter:drop-shadow(0 0 4px rgba(0,255,65,.5))}\
header.top button.fs .ic-compress{display:none}\
header.top button.fs.on .ic-expand{display:none}\
header.top button.fs.on .ic-compress{display:block}\
nav.crumbs{color:var(--mx-dim);margin:.5rem 0 1.6rem;font-size:.9rem;letter-spacing:.5px}\
nav.crumbs::before{content:'~/ ';color:rgba(0,255,65,.4)}\
ul.list{list-style:none;padding:0}\
ul.list li{padding:.7rem .3rem;border-bottom:1px solid rgba(0,255,65,.16);transition:background .15s,box-shadow .15s}\
ul.list li::before{content:'> ';color:var(--mx);text-shadow:0 0 6px var(--mx)}\
ul.list li:hover{background:rgba(0,255,65,.06);box-shadow:inset 2px 0 0 var(--mx)}\
.thumb{color:var(--mx-dim);font-size:.88rem}\
.markdown-body{line-height:1.78}\
.markdown-body p{margin:.85rem 0}\
.markdown-body h1,.markdown-body h2,.markdown-body h3,.markdown-body h4,.markdown-body h5,.markdown-body h6{font-family:'JetBrains Mono',ui-monospace,monospace;font-weight:700;color:#9bffbf;text-shadow:0 0 8px rgba(0,255,65,.4);text-transform:none;letter-spacing:0;line-height:1.3;margin:1.7rem 0 .7rem}\
.markdown-body h1{font-size:1.5rem;border-bottom:1px solid rgba(0,255,65,.28);padding-bottom:.3rem}\
.markdown-body h1::before{content:none}\
.markdown-body h2{font-size:1.25rem;border-bottom:1px solid rgba(0,255,65,.18);padding-bottom:.25rem}\
.markdown-body h3{font-size:1.1rem;color:#7dffa0}\
.markdown-body strong{font-weight:700;color:#f0fff0;text-shadow:0 0 9px rgba(0,255,65,.6)}\
.markdown-body em{font-style:italic;color:#6effc9}\
.markdown-body del{color:rgba(0,255,65,.4);text-decoration:line-through}\
.markdown-body a{color:var(--mx-hi);text-decoration:underline;text-decoration-color:rgba(0,255,65,.5);text-underline-offset:2px}\
.markdown-body a:hover{text-decoration-color:var(--mx)}\
.markdown-body ul{list-style:none;padding-left:1.4rem;margin:.85rem 0}\
.markdown-body ul li{position:relative;margin:.35rem 0}\
.markdown-body ul li::before{content:'\\25b8';color:var(--mx);position:absolute;left:-1.3rem;text-shadow:0 0 6px var(--mx)}\
.markdown-body ul li:has(input)::before{content:none}\
.markdown-body ol{padding-left:1.7rem;margin:.85rem 0}\
.markdown-body ol li{margin:.35rem 0}\
.markdown-body ol li::marker{color:var(--mx)}\
.markdown-body input[type=checkbox]{accent-color:var(--mx);margin-right:.45rem;transform:translateY(1px)}\
.markdown-body blockquote{border-left:3px solid var(--mx);margin:1.1rem 0;padding:.5rem 1rem;color:rgba(0,255,65,.78);background:rgba(0,255,65,.04);box-shadow:inset 0 0 24px rgba(0,255,65,.05)}\
.markdown-body hr{border:none;height:1px;margin:1.8rem 0;background:linear-gradient(90deg,transparent,var(--mx),transparent);box-shadow:0 0 8px rgba(0,255,65,.4)}\
.markdown-body table{border-collapse:collapse;margin:1rem 0;width:100%}\
.markdown-body th,.markdown-body td{border:1px solid rgba(0,255,65,.3);padding:.45rem .8rem;text-align:left}\
.markdown-body th{background:rgba(0,255,65,.1);color:var(--mx-hi);font-weight:700}\
.markdown-body tr:nth-child(even) td{background:rgba(0,255,65,.03)}\
.markdown-body pre{background:rgba(0,18,6,.85);padding:.9rem 1rem;border-radius:4px;overflow:auto;border:1px solid rgba(0,255,65,.35);box-shadow:0 0 18px rgba(0,255,65,.12),inset 0 0 30px rgba(0,255,65,.04);margin:1rem 0}\
.markdown-body code{font-family:'JetBrains Mono',ui-monospace,monospace;background:rgba(0,255,65,.1);padding:.12rem .35rem;border-radius:3px;color:var(--mx-hi);border:1px solid rgba(0,255,65,.18)}\
.markdown-body pre code{background:none;padding:0;border:none;color:#7dff9e}\
.marker{color:#7dffa0;font-size:.85rem;margin-bottom:1.2rem;letter-spacing:1px;text-transform:uppercase;display:inline-block;padding:.35rem .8rem;border:1px solid rgba(0,255,65,.4);border-radius:3px;background:rgba(0,255,65,.05);box-shadow:0 0 12px rgba(0,255,65,.14)}\
.marker::before{content:'>> ';color:var(--mx)}\
.reader-layout{display:flex;gap:1.5rem;align-items:flex-start}\
.piece-nav{flex:0 0 16rem;position:sticky;top:1rem;max-height:90vh;overflow:auto;border:1px solid rgba(0,255,65,.3);border-radius:4px;padding:.5rem;background:rgba(0,12,4,.6);box-shadow:0 0 18px rgba(0,255,65,.1)}\
.reader-main{flex:1;min-width:0}\
.piece{display:none}.piece h2{margin-top:0}\
.nav-item{display:flex;gap:.5rem;align-items:baseline;padding:.45rem .6rem;border-radius:3px;color:var(--mx-soft);cursor:pointer;border-left:2px solid transparent}\
.nav-item:hover{background:rgba(0,255,65,.08);text-decoration:none;border-left-color:rgba(0,255,65,.5)}\
.nav-item.active{background:rgba(0,255,65,.14);color:var(--mx-hi);border-left-color:var(--mx);text-shadow:0 0 8px rgba(0,255,65,.7)}\
.nav-num{color:rgba(0,255,65,.5);font-variant-numeric:tabular-nums;min-width:1.6rem;text-align:right}\
.nav-num::before{content:'['}.nav-num::after{content:']'}\
.nav-item.active .nav-num{color:var(--mx)}\
.nav-title{flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}\
.nav{margin-top:1.8rem;display:flex;gap:1rem}\
.nav button{font-family:inherit;font-size:.95rem;padding:.5rem 1.1rem;cursor:pointer;background:rgba(0,255,65,.06);color:#7dffa0;border:1px solid rgba(0,255,65,.4);border-radius:3px;letter-spacing:1px;text-transform:uppercase;text-shadow:0 0 6px rgba(0,255,65,.4);transition:background .15s,box-shadow .15s,color .15s}\
.nav button:hover{background:rgba(0,255,65,.18);color:var(--mx-hi);box-shadow:0 0 14px rgba(0,255,65,.4)}\
.nav button:active{transform:translateY(1px)}\
@media(max-width:48rem){.reader-layout{flex-direction:column}.piece-nav{position:static;flex:1 1 auto;width:100%;max-height:18rem}}";

// digital rain: a fixed full-screen canvas of falling katakana/hex glyphs,
// drawn dim behind the content (z-index 0, low opacity via CSS).
const MATRIX_JS: &str = "(function(){\
var c=document.createElement('canvas');c.id='matrix-rain';\
document.body.insertBefore(c,document.body.firstChild);\
var x=c.getContext('2d');\
var g='\\u30a2\\u30a4\\u30a6\\u30a8\\u30aa\\u30ab\\u30ad\\u30af\\u30b1\\u30b3\\u30b5\\u30b7\\u30b9\\u30bb\\u30bd\\u30bf\\u30c1\\u30c4\\u30c6\\u30c8\\u30ca\\u30cb\\u30cc\\u30cd\\u30ce\\u30cf\\u30d2\\u30d5\\u30d8\\u30db0123456789ABCDEF=*+<>:.'.split('');\
var fs=14,cols,y;\
function size(){c.width=window.innerWidth;c.height=window.innerHeight;cols=Math.ceil(c.width/fs);y=[];for(var i=0;i<cols;i++){y[i]=Math.floor(Math.random()*-40);}}\
size();window.addEventListener('resize',size);\
function frame(){\
x.fillStyle='rgba(0,0,0,0.06)';x.fillRect(0,0,c.width,c.height);\
x.font=fs+\"px 'Share Tech Mono',monospace\";x.fillStyle='#5dff8a';\
for(var i=0;i<cols;i++){\
x.fillText(g[Math.floor(Math.random()*g.length)],i*fs,y[i]*fs);\
if(y[i]*fs>c.height&&Math.random()>0.98){y[i]=0;}\
y[i]++;}}\
setInterval(frame,55);\
})();";

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

// two corner-bracket icons inside the top-bar toggle button: expand (enter
// full screen) shown by default, compress (exit) shown when `.on` is set.
const FS_ICONS: &str = "\
<svg class='ic ic-expand' viewBox='0 0 24 24' width='20' height='20' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><path d='M8 3H5a2 2 0 0 0-2 2v3'/><path d='M21 8V5a2 2 0 0 0-2-2h-3'/><path d='M3 16v3a2 2 0 0 0 2 2h3'/><path d='M16 21h3a2 2 0 0 0 2-2v-3'/></svg>\
<svg class='ic ic-compress' viewBox='0 0 24 24' width='20' height='20' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><path d='M8 3v3a2 2 0 0 1-2 2H3'/><path d='M21 8h-3a2 2 0 0 1-2-2V3'/><path d='M3 16h3a2 2 0 0 1 2 2v3'/><path d='M16 21v-3a2 2 0 0 1 2-2h3'/></svg>";

// toggle the whole document in/out of native full screen; swap the icon by
// reacting to fullscreenchange (covers Esc and the F11 key too).
const FS_JS: &str = "(function(){\
var b=document.getElementById('fs-toggle');if(!b)return;\
b.addEventListener('click',function(){\
if(document.fullscreenElement){if(document.exitFullscreen)document.exitFullscreen();}\
else{var e=document.documentElement;if(e.requestFullscreen)e.requestFullscreen();}});\
document.addEventListener('fullscreenchange',function(){\
if(document.fullscreenElement){b.classList.add('on');}else{b.classList.remove('on');}});\
})();";

fn layout(page_title: &str, body: Markup) -> Markup {
    html! {
        (DOCTYPE)
        html lang="en" {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                title { (page_title) " — pieces" }
                link rel="preconnect" href="https://fonts.googleapis.com";
                link rel="preconnect" href="https://fonts.gstatic.com" crossorigin="";
                link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,400;0,600;0,700;1,400;1,700&family=Share+Tech+Mono&display=swap";
                style { (PreEscaped(APP_CSS)) }
            }
            body {
                header.top {
                    a.brand href="/" { "pieces" }
                    button.fs id="fs-toggle" type="button" title="Toggle full screen" aria-label="Toggle full screen" {
                        (PreEscaped(FS_ICONS))
                    }
                }
                main { (body) }
                script { (PreEscaped(MATRIX_JS)) }
                script { (PreEscaped(FS_JS)) }
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
        // top-bar full-screen toggle is part of the shared layout
        assert!(html.contains("id=\"fs-toggle\""));
        assert!(html.contains("ic-expand"));
        assert!(html.contains("ic-compress"));
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
