package cz.bbn.cerberus.dssetting.ui.components;

import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.DsSettingTabComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dssetting.dto.DsSettingDto;
import cz.bbn.cerberus.dssetting.ui.components.tab.DsSettingDetailTabComponent;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class DsSettingDetailComponent
        extends AppDetailCardComponent<DsSettingDto> implements AppBinderOperation<DsSettingDto> {

    private final List<UserDto> userList;

    public DsSettingDetailComponent(DsSettingDto dto, SaveAction<DsSettingDto> saveAction, boolean showSubmitButton,
                                    AppEnv appEnv, List<UserDto> userList,
                                    EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        this.userList = userList;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = getDto().getId() == null ? Transl.get("New ds setting") :
                Transl.get("DS setting")
                        .concat(" - ")
                        .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(AdministrationView.ROUTE + "/" + DsSettingTabComponent.TAB_INDEX);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.DS_SETTING_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        add(new DsSettingDetailTabComponent(this, userList, false, getAppEnv(), !isShowSubmitButton()));
    }

}
