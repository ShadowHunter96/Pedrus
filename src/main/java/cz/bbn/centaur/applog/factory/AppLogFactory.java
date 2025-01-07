package cz.bbn.cerberus.applog.factory;

import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.applog.persistance.AppLogEntity;
import cz.bbn.cerberus.user.factory.UserFactory;

public class AppLogFactory {

    private AppLogFactory() {
    }

    public static AppLogDto fromEntity(AppLogEntity entity) {
        AppLogDto dto = new AppLogDto();
        dto.setAction(entity.getAction());
        dto.setDate(entity.getDate());
        dto.setMessage(entity.getMessage());
        dto.setAppId(entity.getAppId());
        if(entity.getUserEntity() != null){
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        return dto;
    }
}
