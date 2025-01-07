package cz.bbn.cerberus.custompermission.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface CustomUserPermissionRepository extends JpaRepository<CustomUserPermissionEntity, Long> {

    @Query("select customPermission from CustomUserPermissionEntity customPermission " +
            "where customPermission.objectName like :object and customPermission.userId = :userId " +
            "and customPermission.objectId like :objectId")
    Set<CustomUserPermissionEntity> getPermissionByObjectAndUser(String object, Long userId, String objectId);

    @Query("select customPermission from CustomUserPermissionEntity customPermission " +
            "where customPermission.objectName like :object and customPermission.userId = :userId " +
            "and customPermission.permissionId like :permission")
    CustomUserPermissionEntity getPermissionStringByObjectUserAndId(String object, Long userId, String permission);

    @Query("select entity.userId from CustomUserPermissionEntity entity " +
            "where entity.permissionId like :id " +
            "and (entity.objectName like :type or entity.objectName like '-- ALL --')")
    Set<Long> getUserNameSetByObjectIdObjectType(String id, String type);

    @Query("select entity from CustomUserPermissionEntity entity where entity.userId = :userId")
    Set<CustomUserPermissionEntity> getByUserId(Long userId);

    @Query("select entity from CustomUserPermissionEntity entity " +
            "where entity.objectName = :objectName and entity.objectId = :objectId " +
            "and entity.permissionId = :permissionId")
    Set<CustomUserPermissionEntity> getPermissionByObjectNameObjectIdPermissionId(String objectName, String objectId,
                                                                                  String permissionId);

    @Query("select entity from CustomUserPermissionEntity entity where entity.permissionId = :id")
    Set<CustomUserPermissionEntity> getByPermissionId(String id);

    void deleteByUserIdAndObjectName(Long userId, String objectName);
}
