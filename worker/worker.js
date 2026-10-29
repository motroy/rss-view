// Feed proxy: GET /?url=<feed url> returns the feed body with CORS headers.

const PRIVATE_HOST = /^(localhost|.*\.local|.*\.internal|\[.*\]|\d{1,3}(\.\d{1,3}){3})$/i;

export default {
  async fetch(request) {
    const cors = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, OPTIONS',
      'Access-Control-Allow-Headers': '*'
    };
    if (request.method === 'OPTIONS') return new Response(null, { status: 204, headers: cors });
    if (request.method !== 'GET') return new Response('Method not allowed', { status: 405, headers: cors });

    const target = new URL(request.url).searchParams.get('url');
    let feedUrl;
    try {
      feedUrl = new URL(target);
    } catch (_) {
      return new Response('Missing or invalid url parameter', { status: 400, headers: cors });
    }
    if (!/^https?:$/.test(feedUrl.protocol) || PRIVATE_HOST.test(feedUrl.hostname)) {
      return new Response('URL not allowed', { status: 400, headers: cors });
    }

    try {
      const upstream = await fetch(feedUrl.toString(), {
        headers: {
          'User-Agent': 'rss-view feed proxy',
          'Accept': 'application/rss+xml, application/atom+xml, application/xml, text/xml, */*'
        },
        redirect: 'follow',
        cf: { cacheTtl: 300, cacheEverything: true }
      });
      const headers = new Headers(cors);
      headers.set('Content-Type', upstream.headers.get('Content-Type') || 'application/xml');
      headers.set('Cache-Control', 'public, max-age=300');
      return new Response(upstream.body, { status: upstream.status, headers });
    } catch (e) {
      return new Response('Upstream fetch failed: ' + e.message, { status: 502, headers: cors });
    }
  }
};
