package it.unipi.dsmt.FitConnect.config;

import it.unipi.dsmt.FitConnect.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.*;
import org.springframework.core.env.Environment;
import org.springframework.data.ldap.repository.config.EnableLdapRepositories;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.LdapContextSource;

@Configuration
@PropertySource("classpath:application.properties")
@ComponentScan(basePackages = {"it.unipi.dsmt.FitConnect.*"})
@Profile("default")
@EnableLdapRepositories(basePackages = "it.unipi.dsmt.FitConnect.repositories.*")
public class AppConfig {

    @Autowired
    private Environment env;

    @Bean
    public LdapContextSource contextSource() {

        LdapContextSource contextSource = new LdapContextSource();
        contextSource.setUrl("ldap://localhost:389"); // Adjust the URL based on your LDAP server configuration
        contextSource.setUserDn("cn=admin,dc=baeldung,dc=com"); // Adjust the DN based on your LDAP server configuration
        contextSource.setPassword("secret"); // Adjust the password based on your LDAP server configuration
        return contextSource;
    }

    @Bean
    public LdapTemplate ldapTemplate() {
        return new LdapTemplate(contextSource());
    }

    @Bean
    public UserService ldapClient() {
        return new UserService();
    }

}
