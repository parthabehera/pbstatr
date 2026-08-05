#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Generate the PbStatR hex-sticker logo.
#
# The logo is authored as SVG (for crisp scaling) and rasterised to PNG at two
# sizes: logo.png (240px, for the README/pkgdown navbar) and logo-hex.png
# (1040px, hi-res for docs/printing). It depicts the package's themes: a rising
# sun, a GWAS scatter with a trend line, a DNA double helix, and an ascending
# growth-curve bar chart, all in the PbStatR palette.
#
# Requires: numpy, cairosvg  (pip install numpy cairosvg)
# Run from the package root:  python3 data-raw/make_logo.py
# ---------------------------------------------------------------------------
import numpy as np, math, os

np.random.seed(7)
MAIN = ["#2E9FDF", "#E7B800", "#FC4E07", "#00AF66", "#8E44AD",
        "#E84393", "#16A085", "#D35400"]
FIELD = ["#2E9FDF", "#5AAE61", "#00AF66", "#E6C200", "#E7B800", "#1B7837",
         "#16A085", "#C9A227", "#FC4E07", "#A6DBA0", "#8E44AD"]
GOLD = "#FDE725"; DKGREEN = "#0C3D22"
S = 520; cx = S / 2; R = 252


def hex_flat(cx, cy, r):
    return [(cx + r * math.cos(math.radians(a)), cy + r * math.sin(math.radians(a)))
            for a in [0, 60, 120, 180, 240, 300]]


def ptstr(p):
    return " ".join(f"{x:.1f},{y:.1f}" for x, y in p)


outer = hex_flat(cx, cx, R)
svg = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {S} {S}">']
svg.append('''<defs>
<linearGradient id="bg" x1="0%" y1="0%" x2="60%" y2="100%">
  <stop offset="0%" stop-color="#0B5A34"/><stop offset="48%" stop-color="#12876A"/>
  <stop offset="100%" stop-color="#1C6EA6"/></linearGradient>
<radialGradient id="sun" cx="50%" cy="50%" r="50%">
  <stop offset="0%" stop-color="#FFF3C4" stop-opacity="1"/>
  <stop offset="45%" stop-color="#E7B800" stop-opacity="0.55"/>
  <stop offset="100%" stop-color="#E7B800" stop-opacity="0"/></radialGradient>
<linearGradient id="textband" x1="0%" y1="0%" x2="0%" y2="100%">
  <stop offset="0%" stop-color="#08301B" stop-opacity="0"/>
  <stop offset="30%" stop-color="#08301B" stop-opacity="0.5"/>
  <stop offset="70%" stop-color="#08301B" stop-opacity="0.5"/>
  <stop offset="100%" stop-color="#08301B" stop-opacity="0"/></linearGradient>
<filter id="shadow" x="-20%" y="-20%" width="140%" height="140%">
  <feDropShadow dx="0" dy="6" stdDeviation="10" flood-color="#000" flood-opacity="0.38"/></filter>
</defs>''')
svg.append(f'<clipPath id="hc"><polygon points="{ptstr(outer)}"/></clipPath>')
svg.append(f'<polygon points="{ptstr(hex_flat(cx, cx, R + 6))}" fill="{DKGREEN}" filter="url(#shadow)"/>')
svg.append('<g clip-path="url(#hc)">')
svg.append(f'<rect width="{S}" height="{S}" fill="url(#bg)"/>')
svg.append('<circle cx="168" cy="150" r="112" fill="url(#sun)"/>'
           '<circle cx="168" cy="150" r="26" fill="#FFF3C4"/>')

# GWAS scatter arc
xs = np.linspace(100, 424, 22); base = np.linspace(112, 76, 22)
svg.append(f'<line x1="100" y1="110" x2="424" y2="72" stroke="{GOLD}" '
           'stroke-width="3.4" stroke-opacity="0.85" stroke-dasharray="1.5,7" '
           'stroke-linecap="round"/>')
for i, x in enumerate(xs):
    peak = np.random.rand() < 0.18
    y = base[i] - (np.random.uniform(14, 28) if peak else np.random.uniform(0, 6))
    svg.append(f'<circle cx="{x:.0f}" cy="{y:.0f}" r="{5.2 if peak else 4.0:.1f}" '
               f'fill="{MAIN[i % 8]}" fill-opacity="0.95"/>')

# DNA helix backdrop (tapered through the text zone)
hx = cx; top = 158; bot = 360; turns = 2.5; amp = 26; n = 90
A = []; B = []; rungs = []
for k in range(n):
    t = k / (n - 1); yy = top + (bot - top) * t; ph = t * turns * 2 * math.pi
    zone = 0.45 if 236 < yy < 300 else 1.0
    a2 = amp * zone
    A.append((hx + a2 * math.sin(ph), yy)); B.append((hx - a2 * math.sin(ph), yy))
    if k % 6 == 0 and not (236 < yy < 300):
        rungs.append((hx + a2 * math.sin(ph), hx - a2 * math.sin(ph), yy))
pth = lambda p: "M " + " L ".join(f"{x:.1f} {y:.1f}" for x, y in p)
for xa, xb, yy in rungs:
    svg.append(f'<line x1="{xa:.1f}" y1="{yy:.1f}" x2="{xb:.1f}" y2="{yy:.1f}" '
               'stroke="#FFFFFF" stroke-opacity="0.2" stroke-width="2"/>')
svg.append(f'<path d="{pth(A)}" fill="none" stroke="{GOLD}" stroke-width="4.2" '
           'stroke-opacity="0.5" stroke-linecap="round"/>')
svg.append(f'<path d="{pth(B)}" fill="none" stroke="#9BDCF7" stroke-width="4.2" '
           'stroke-opacity="0.5" stroke-linecap="round"/>')

# growth bars
baseline = 404; nb = 11; bx0, bx1 = 100, 420; bw = (bx1 - bx0) / nb * 0.6
heights = np.linspace(26, 104, nb); tops = []
for i in range(nb):
    x = bx0 + (bx1 - bx0) * i / nb; h = heights[i]
    svg.append(f'<rect x="{x:.0f}" y="{baseline - h:.0f}" width="{bw:.0f}" '
               f'height="{h:.0f}" rx="3" fill="{FIELD[i % 11]}" fill-opacity="0.9"/>')
    svg.append(f'<circle cx="{x + bw / 2:.0f}" cy="{baseline - h - 6:.0f}" r="3.2" fill="{GOLD}"/>')
    tops.append(f"{x + bw / 2:.0f},{baseline - h - 6:.0f}")
svg.append(f'<polyline points="{" ".join(tops)}" fill="none" stroke="{GOLD}" '
           'stroke-width="2" stroke-opacity="0.45" stroke-dasharray="1,4"/>')
svg.append('</g>')

svg.append('<g clip-path="url(#hc)"><rect x="0" y="224" width="520" height="92" '
           'fill="url(#textband)"/></g>')
svg.append(f'<polygon points="{ptstr(hex_flat(cx, cx, R - 7))}" fill="none" '
           f'stroke="{GOLD}" stroke-width="4" stroke-opacity="0.92"/>')
svg.append(f'<polygon points="{ptstr(hex_flat(cx, cx, R))}" fill="none" '
           f'stroke="{DKGREEN}" stroke-width="9"/>')
svg.append(f'<text x="{cx}" y="290" font-family="Georgia,serif" font-size="74" '
           f'font-weight="bold" fill="#FFFFFF" text-anchor="middle" stroke="{DKGREEN}" '
           'stroke-width="1.6" paint-order="stroke" letter-spacing="1">PbStatR</text>')
svg.append(f'<text x="{cx}" y="442" font-family="Helvetica,Arial,sans-serif" '
           f'font-size="17" font-weight="bold" fill="{GOLD}" text-anchor="middle" '
           f'letter-spacing="4.5" stroke="{DKGREEN}" stroke-width="0.6" '
           'paint-order="stroke">PLANT BREEDING STATS</text>')
svg.append('</svg>')

os.makedirs("man/figures", exist_ok=True)
with open("man/figures/logo.svg", "w") as f:
    f.write("\n".join(svg))

try:
    import cairosvg
    cairosvg.svg2png(url="man/figures/logo.svg",
                     write_to="man/figures/logo.png",
                     output_width=240, output_height=240)
    cairosvg.svg2png(url="man/figures/logo.svg",
                     write_to="man/figures/logo-hex.png",
                     output_width=1040, output_height=1040)
    print("Wrote logo.svg, logo.png (240px), logo-hex.png (1040px)")
except ImportError:
    print("Wrote logo.svg. Install cairosvg to also render the PNGs, "
          "or open the SVG in any editor to export.")
