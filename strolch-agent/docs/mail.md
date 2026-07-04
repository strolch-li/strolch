### Mail Handler

The `MailHandler` component provides an API for sending email notifications from a Strolch application.

#### Implementations

-   **`SmtpMailHandler`**: Sends real emails via an SMTP server.
-   **`SimulatedMailHandler`**: Logs emails to the console or a file instead of sending them. Useful for development and testing.

#### Features

-   **Synchronous and Asynchronous Sending**: Send emails immediately or in the background.
-   **Attachments**: Support for adding multiple attachments to emails.
-   **Encryption and Signing**: Support for sending encrypted and signed emails (typically using PGP or S/MIME, depending on the configuration).

#### Sending an Email

```java
MailHandler mailHandler = agent.getComponent(MailHandler.class);

mailHandler.sendMailAsync(
    "recipient@example.com",
    "Subject line",
    "Body text of the email"
);
```

#### Sending with Attachments

```java
MailAttachment attachment = new MailAttachment("report.pdf", "application/pdf", pdfData);
mailHandler.sendMailWithAttachmentAsync(
    "recipient@example.com",
    "Report",
    "Please find the report attached.",
    attachment
);
```

#### Configuration

The `MailHandler` is configured in `StrolchConfiguration.xml`.

```xml
<Component>
    <name>MailHandler</name>
    <api>li.strolch.handler.mail.MailHandler</api>
    <impl>li.strolch.handler.mail.SmtpMailHandler</impl>
    <Properties>
        <fromAddr>Strolch System &lt;user@example.com&gt;</fromAddr>
        <username>user@example.com</username>
        <password>XXX</password>
        <auth>true</auth>
        <startTls>true</startTls>
        <host>smtp.example.com</host>
        <port>587</port>
        <sign>true</sign>
        <encrypt>true</encrypt>
        <!-- file must exist in config/ -->
        <signingKey>user@example.com.key</signingKey>
        <signingKeyPassword>XXX</signingKeyPassword>
        <!-- comma separated list of files in config/ -->
        <recipientPublicKeys>eitch@eitchnet.ch.asc</recipientPublicKeys>
    </Properties>
</Component>
```

To use the simulation mode:

```xml
<Component>
    <name>MailHandler</name>
    <api>li.strolch.handler.mail.MailHandler</api>
    <impl>li.strolch.handler.mail.SimulatedMailHandler</impl>
</Component>
```
