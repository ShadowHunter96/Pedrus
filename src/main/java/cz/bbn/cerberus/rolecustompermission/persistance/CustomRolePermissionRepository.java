package cz.bbn.cerberus.rolecustompermission.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface CustomRolePermissionRepository extends JpaRepository<CustomRolePermissionEntity, Long> {

    @Query("select customPermission from CustomRolePermissionEntity customPermission " +
            "where customPermission.objectName like :object and customPermission.roleId = :roleId " +
            "and customPermission.objectId like :objectId")
    Set<CustomRolePermissionEntity> getPermissionByObjectAndRole(String object, String roleId, String objectId);

    @Query("select entity from CustomRolePermissionEntity entity where entity.roleId = :roleId")
    Set<CustomRolePermissionEntity> getByRoleId(String roleId);

    void deleteByRoleIdAndObjectName(String id, String object);
}
