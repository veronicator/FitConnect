package it.unipi.dsmt.FitConnect.repositories;

import it.unipi.dsmt.FitConnect.entities.User;
import org.springframework.data.ldap.repository.LdapRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
@Repository
public interface LdapUserRepository extends LdapRepository<User> {
    User findByUsername(String username);

    User findByUsernameAndPassword(String username, String password);

    List<User> findByUsernameLikeIgnoreCase(String username);

}
