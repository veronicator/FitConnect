package it.unipi.dsmt.fitconnect.config;

import it.unipi.dsmt.fitconnect.services.AuthService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.*;
import org.springframework.data.ldap.repository.config.EnableLdapRepositories;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.LdapContextSource;

@Configuration
@PropertySource("classpath:application.properties")
@ComponentScan(basePackages = {"it.unipi.dsmt.fitconnect.*"})
@Profile("default")
@EnableLdapRepositories(basePackages = "it.unipi.dsmt.fitconnect.repositories.ldap.*")
public class LdapConfig {

    @Value("${spring.ldap.urls}")
    private String url;
    @Value("${spring.ldap.username}")
    private String userDn;
    @Value("${spring.ldap.password}")
    private String pwd;

    @Bean
    public LdapContextSource contextSource() {

        LdapContextSource contextSource = new LdapContextSource();

        contextSource.setUrl(url); // Adjust the URL based on your LDAP server configuration
        contextSource.setUserDn(userDn); // Adjust the DN based on your LDAP server configuration
        contextSource.setPassword(pwd); // Adjust the password based on your LDAP server configuration

        return contextSource;
    }

    @Bean
    public LdapTemplate ldapTemplate() {
        return new LdapTemplate(contextSource());
    }

    @Bean
    public AuthService ldapClient() {
        return new AuthService();
    }

}
