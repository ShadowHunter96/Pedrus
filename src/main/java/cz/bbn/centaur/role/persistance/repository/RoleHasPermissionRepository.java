package cz.bbn.cerberus.role.persistance.repository;

import cz.bbn.cerberus.role.persistance.entity.RoleHasPermissionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface RoleHasPermissionRepository extends JpaRepository<RoleHasPermissionEntity, Long> {

    @Modifying
    @Query("delete from RoleHasPermissionEntity entity where entity.id.roleId = :roleId")
    void deleteByRoleId(String roleId);

    @Query("select entity.id.permissionId from RoleHasPermissionEntity entity where entity.id.roleId = :roleId")
    Set<String> getPermissionByRoleId(String roleId);

    @Query("select entity from RoleHasPermissionEntity entity where entity.id.roleId = :roleId")
    Set<RoleHasPermissionEntity> getByRoleId(String roleId);
}
