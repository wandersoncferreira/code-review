# Merge

For now the three strategies are calling the correct endpoint at GITHUB however
we do not provide any way to specify the commit title and commit message that
will be used.

The current process is going to use the `description` and `title` of the PR to
fill these required values.

The next steps in this feature will be:

- [ ] allow customization
- [ ] perform request asynchronously
