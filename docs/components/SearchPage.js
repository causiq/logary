// search
import Link from 'next/link'
import Head from 'next/head'

const SearchPage = ({ query, results, setQuery }) => {
  return <>
    <Head>
      <title>Logary v5 — Professional logging, metrics and analytics for your apps</title>
    </Head>
    <section className="search-section text-center">
      <div className="container">
        <h2 className="title">Search results for '{query}'</h2>
        <div className="results">
          {results.length > 0
            ? <ol>
                {results.map((r, i) =>
                  <li key={r.id}>
                    <Link href={r.href}>
                      <a onClick={_ => setQuery('')}>
                        {r.title}
                      </a>
                    </Link>
                    {r.description != null
                      ? <span className="description">{` — ${r.description}`}</span>
                      : null}
                  </li>
                )}
              </ol>
            : "No search results"}
        </div>
      </div>
    </section>
  </>
}

export default SearchPage