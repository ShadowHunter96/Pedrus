package cz.bbn.cerberus.dssetting.factory;

import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.persistance.DsSettingEntity;
import cz.bbn.cerberus.dssetting.persistance.DsSettingSimpleEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

import java.io.IOException;

public class DsSettingFactory {

    private DsSettingFactory() {
    }

    public static DsSettingSimpleDto fromEntity(DsSettingSimpleEntity entity) {
        DsSettingSimpleDto dto = new DsSettingSimpleDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setDeleted(entity.getDeleted());
        dto.setUserId((entity.getUserId()));
        return dto;
    }

    public static DsSettingDto fromEntity(DsSettingEntity entity) {
        DsSettingDto dto = new DsSettingDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setLoginByCertificate(entity.getLoginByCertificate());
        dto.setLogin(entity.getLogin());
        dto.setPassword(entity.getPassword());
        dto.setDeleted(entity.getDeleted());

        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        return dto;
    }

    public static void fillEntity(DsSettingEntity entity, DsSettingDto dto) throws IOException {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setLogin(dto.getLogin());
        entity.setLoginByCertificate(dto.isLoginByCertificate());
        entity.setDeleted(dto.getDeleted());
        if (dto.isLoginByCertificate()) {
            entity.setCertificate(dto.getCertificate().readAllBytes());
            entity.setLogin("");
            entity.setPassword("");
        } else {
            entity.setPassword(dto.getPassword());
            entity.setDeleted(dto.getDeleted());
            entity.setCertificate(null);
        }

        if (dto.getUserDto() != null) {
            UserEntity userEntity = new UserEntity();
            UserFactory.fillEntity(userEntity, dto.getUserDto());
            entity.setUserEntity(userEntity);
        }
    }
}
