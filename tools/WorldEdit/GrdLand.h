#ifndef __GRDLAND_INCLUDED__
#define __GRDLAND_INCLUDED__

#include "ChnkIO.h"

class CGrdTileIO : public CChnkIO {
public:
	CGrdTileIO(void);
	~CGrdTileIO(void);
	void SetTextureID(DWORD TMapID);
	void SetVertexFlip(DWORD VertexFlip);
	void SetTextureFlip(DWORD TextureFlip);
	void SetFlags(DWORD Flags) { m_Flags = Flags; }
	void SetVertexHeight(DWORD Index,float y);
	BOOL Write(FILE *Stream);
	DWORD GetTextureID(void);
	DWORD GetVertexFlip(void);
	DWORD GetTextureFlip(void);
	DWORD GetFlags(void) { return m_Flags; }
	float GetVertexHeight(DWORD Index);
	BOOL Read(FILE *Stream);
protected:
	DWORD m_TMapID;
	DWORD m_VertexFlip;
	DWORD m_TextureFlip;
	DWORD m_Flags;
	float m_Height[4];
};

class CGrdLandIO : public CChnkIO {
public:
	CGrdLandIO(void);
	~CGrdLandIO(void);
	void SetCameraPosition(float x,float y,float z);
	void SetCameraRotation(float x,float y,float z);
	void SetSeaLevel(DWORD SeaLevel) { m_SeaLevel = SeaLevel; }
	void Set2DPosition(SLONG ScrollX,SLONG ScrollY);
	void SetHeightScale(DWORD HeightScale);
	void SetTextureSize(DWORD TextureWidth,DWORD TextureHeight);
	void SetMapSize(DWORD MapWidth,DWORD MapHeight);
	void SetTileSize(DWORD TileWidth,DWORD TileHeight);
	void SetNumTextures(DWORD NumTextures);
	void SetTextureName(DWORD NameIndex,char *TextureName);
	void SetSnapMode(DWORD SnapMode) { m_SnapMode = SnapMode; }
	void SetSnapX(DWORD SnapX) { m_SnapX = SnapX; }
	void SetSnapZ(DWORD SnapZ) { m_SnapZ = SnapZ; }
	void SetGravity(BOOL EnableGravity) { m_EnableGravity = EnableGravity; }
	CGrdTileIO *GetTile(DWORD Index);
	BOOL Write(FILE *Stream);
	void GetCameraPosition(float *x,float *y,float *z);
	void GetCameraRotation(float *x,float *y,float *z);
	DWORD GetSeaLevel(void) { return m_SeaLevel; }
	void Get2DPosition(SLONG *ScrollX,SLONG *ScrollY);
	DWORD GetHeightScale(void);
	void GetTextureSize(DWORD *TextureWidth,DWORD *TextureHeight);
	void GetMapSize(DWORD *MapWidth,DWORD *MapHeight);
	void GetTileSize(DWORD *TileWidth,DWORD *TileHeight);
	DWORD GetNumTextures(void);
	char *GetTextureName(DWORD NameIndex);
	DWORD GetSnapMode(void) { return m_SnapMode; }
	DWORD GetSnapX(void) { return m_SnapX; }
	DWORD GetSnapZ(void) { return m_SnapZ; }
	BOOL GetGravity(void) { return (BOOL)m_EnableGravity; }
	DWORD GetVersion(void) { return m_Version; }
	BOOL Read(FILE *Stream);
protected:
	DWORD m_Version;
	float m_CameraXPos;
	float m_CameraYPos;
	float m_CameraZPos;
	float m_CameraXRot;
	float m_CameraYRot;
	float m_CameraZRot;
	DWORD m_SeaLevel;
	DWORD m_SnapX;
	DWORD m_SnapZ;
	DWORD m_SnapMode;
	DWORD m_EnableGravity;
	SLONG m_ScrollX;
	SLONG m_ScrollY;
	DWORD m_HeightScale;
	DWORD m_MapWidth;
	DWORD m_MapHeight;
	DWORD m_TileWidth;
	DWORD m_TileHeight;
	DWORD m_NumTiles;
	DWORD m_TextureWidth;
	DWORD m_TextureHeight;
	DWORD m_NumTextures;
	char **m_TextureNames;
	CGrdTileIO **m_Tiles;
};

#endif