package cz.bbn.cerberus.assetposition.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.assetposition.dto.AssetPositionFilterDto;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;

public class AssetPositionFilterComponent extends FormLayout {

    private final Button search;

    private TextField id;
    private TextField name;

    public AssetPositionFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    private void initComponent() {
        id = new TextField(Transl.get("Id"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public AssetPositionFilterDto getAssetPositionFilterDto() {
        AssetPositionFilterDto assetPositionFilterDto = new AssetPositionFilterDto();
        assetPositionFilterDto.setName(name.getValue());
        assetPositionFilterDto.setId(id.getValue());
        return assetPositionFilterDto;
    }
}
