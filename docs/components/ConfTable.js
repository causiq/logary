export default function ConfList(props) {
  return <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Required</th>
          <th>Default value</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        {props.conf.map(x =>
          <tr key={x.name}>
            <td><code>{x.name}</code></td>
            <td><code>{x.type}</code></td>
            <td>{x.required ? "yes" : "no"}</td>
            <td><code>{x.default}</code></td>
            <td>{x.description}</td>
          </tr>
        )}
      </tbody>
    </table>
}