package it.unipi.dsmt.fitconnect.repositories.ldap;

import it.unipi.dsmt.fitconnect.entities.LdapUser;
import org.springframework.data.ldap.repository.LdapRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
@Repository("ldapUserRepository")
public interface LdapUserRepository extends LdapRepository<LdapUser> {
    LdapUser findByUsername(String username);

    LdapUser findByUsernameAndPassword(String username, String password);

    boolean existsByUsername(String username);

    List<LdapUser> findByUsernameLikeIgnoreCase(String username);

}
