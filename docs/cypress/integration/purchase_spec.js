describe('purchase', function() {
  beforeEach(() => {
    cy.visit('http://localhost:3000/other/pricing')
  })

  it('4242 card', function() {
    cy.get('[data-cy=customer-companyName]').type("Test company")
    cy.get('[data-cy=customer-vatNo]').type("SE556111-1111")
    cy.get('[data-cy=customer-name]').type("Person Persson")
    cy.get('[data-cy=customer-email]').type("person@example.com")
    // for later https://gist.github.com/mbrochh/460f6d4fce959791c8f947cb30bed6a7
    //cy.get('.__PrivateStripeElement > iframe').then($i => {
      //const $body = $i.contents().find('body')
      //cy.wrap($body)
        //.find('input[name="cardnumber"]')
        //.type('4242')
        //.type('4242')
        //.type('4242')
        //.type('4242')
      //cy.wrap($body)
        //.find('input:eq(2)')
        //.type('1222')
      //cy.wrap($body)
        //.find('input:eq(3)')
        //.type('223')
      //cy.wrap($body)
        //.find('input:eq(4)')
        //.type('424242')
    //})
  })
})