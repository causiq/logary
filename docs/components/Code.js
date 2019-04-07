const Code = ({ lang, children }) =>
  <pre>
    <code class={`language-${lang}`}>
      {children}
    </code>
  </pre>

const BashCode = ({ children }) =>
  <Code lang="bash">
    {children}
  </Code>

export { BashCode }

export default Code