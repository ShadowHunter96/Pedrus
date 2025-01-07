package cz.bbn.cerberus.role.persistance.repository;

import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface RoleRepository extends JpaRepository<RoleEntity, String> {

    Optional<RoleEntity> findByIdIgnoreCase(String code);

    @Query("select entity.id from RoleEntity entity")
    Set<String> findAllId();

    @Modifying
    @Query("update RoleEntity entity " +
            "set entity.backOffice = false " +
            "where entity.backOffice = true and entity.id != :id")
    void updateBackOffice(String id);

    Optional<RoleEntity> findByBackOffice(boolean backOffice);

    Optional<RoleEntity> findByInfrastructure(boolean infrastructure);

    @Query("select entity from RoleEntity entity where entity.infrastructure = true")
    List<RoleEntity> getByInf();

    @Query("select entity.id from RoleEntity entity where entity.backOffice = true")
    List<String> findBackOfficeRoleId();

    @Query("select entity.id from RoleEntity entity where entity.infrastructure = true")
    List<String> findInfrastructureRoleId();
}
