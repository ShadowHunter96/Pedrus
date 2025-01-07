package cz.bbn.cerberus.dssetting;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.DsSettingTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.dssetting.ui.DsSettingDetailView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.List;

@Component
@Slf4j
public class DsSettingComponentOperation {

    private final DsSettingService dsSettingService;
    private final UserService userService;
    private final AppEnv appEnv;

    public DsSettingComponentOperation(DsSettingService dsSettingService, UserService userService, AppEnv appEnv) {
        this.dsSettingService = dsSettingService;
        this.userService = userService;
        this.appEnv = appEnv;
    }

    public SaveAction<DsSettingDto> getSaveAction(AppDialog dialog) {
        return (newDto, originalDto) -> {
            try {
                if (newDto.isLoginByCertificate() && newDto.getCertificate() == null) {
                    ErrorNotification.show(Transl.get("Some certificate must be selected"), appEnv);
                } else {
                    save(newDto, originalDto, dialog);
                }

            } catch (SystemException | IOException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public List<UserDto> getUserList() {
        return userService.findUserList();
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }

    private void save(DsSettingDto newDto, DsSettingDto originalDto, AppDialog dialog)
            throws SystemException, IOException {
        if (originalDto.getId() != null) {
            dsSettingService.updateDsSetting(newDto, originalDto);
            UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                    .concat(String.valueOf(DsSettingTabComponent.TAB_INDEX)));
        } else {
            newDto.setDeleted(false);
            dsSettingService.saveDsSetting(newDto);
            if (dialog != null) {
                dialog.showWarning(false);
                dialog.close();
                UI.getCurrent().navigate(DsSettingDetailView.ROUTE.concat("/").concat(newDto.getId()));
            }
        }
        SuccessNotification.showSavingSuccess(appEnv);
    }
}
