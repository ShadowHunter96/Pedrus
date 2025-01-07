package cz.bbn.cerberus.asset.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.asset.dto.AssetFilterDto;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class AssetFilterComponent extends FormLayout {

    private TextField id;
    private TextField name;
    private TextField serialNumber;
    private TextField owner;
    private TextField type;
    private Checkbox showDeleted;

    private final Button search;

    public AssetFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    private void initComponent() {
        id = new TextField(Transl.get("Id"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        serialNumber = new TextField(Transl.get("Serial number"));
        this.add(serialNumber);

        owner = new TextField(Transl.get("owner"));
        this.add(owner);

        type = new TextField(Transl.get("type"));
        this.add(type);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.ASSET_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public AssetFilterDto getAssetFilterDto() {
        AssetFilterDto assetFilterDto = new AssetFilterDto();
        assetFilterDto.setId(id.getValue());
        assetFilterDto.setName(name.getValue());
        assetFilterDto.setSerialNumber(serialNumber.getValue());
        assetFilterDto.setOwner(owner.getValue());
        assetFilterDto.setType(type.getValue());
        assetFilterDto.setShowDeleted(showDeleted.getValue());
        return assetFilterDto;
    }
}
