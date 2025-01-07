package cz.bbn.cerberus.user.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface UserRepository extends JpaRepository<UserEntity, Long>, JpaSpecificationExecutor<UserEntity> {

    UserEntity findByLogin(String login);

    @Query("select entity from UserEntity entity where entity.deleted != true")
    List<UserEntity> findAllAllowed();

    @Modifying
    @Query("update UserEntity entity set entity.preferredLanguage = :language where entity.id = :userId")
    void updateLanguage(String language, Long userId);
}
