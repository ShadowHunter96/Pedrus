package cz.bbn.cerberus.dssetting.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dssetting.dto.DsSettingFilterDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class DsSettingFilterComponent extends FormLayout {

    private final Button search;

    private Checkbox showDeleted;

    public DsSettingFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    void initComponent() {
        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.DS_SETTINGS_SHOW_DELETED)) {
            this.add(showDeleted);
            this.add(search);
        }
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public DsSettingFilterDto getDsSettingFilterDto() {
        DsSettingFilterDto dsSettingFilterDto = new DsSettingFilterDto();
        dsSettingFilterDto.setShowDeleted(showDeleted.getValue());
        return dsSettingFilterDto;
    }
}
