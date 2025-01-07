package cz.bbn.cerberus.permission;

import cz.bbn.cerberus.role.dto.RoleHasPermissionDto;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@Service
public class PermissionService {

    public Set<RoleHasPermissionDto> convertPermissionsToDto() {
        Set<RoleHasPermissionDto> dtos = new HashSet<>();
        Arrays.stream(Permission.values()).forEach(permission -> {
            RoleHasPermissionDto dto = new RoleHasPermissionDto();
            dto.setPermissionId(permission.name());
            dtos.add(dto);
        });
        return dtos;
    }
}
