package cz.bbn.cerberus.user.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;

public interface UserActiveRoleRepository extends JpaRepository<UserActiveRoleEntity, UserActiveRoleId> {

    @Modifying
    void deleteByIdUserId(Long userId);
}
